library("lubridate")
library("readr")
library("gridExtra")
#library("gdata")
library("fitdistrplus")
library(here)
library(tidyverse)

BAG_data_dir <- here::here("app", "data", "BAG")
BAG_data_dir_Git <- here::here("../ch-hospital-data/data/CH")

# output_data_dir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data"
outDir <- here::here("app", "data", "CH")

dir.create(outDir, showWarnings = FALSE)

### Find latest BAG data file

bagFiles <- c(
  # polybox
  list.files(BAG_data_dir,
    pattern = "*FOPH_COVID19_data_extract.csv",
    full.names = TRUE,
    recursive = TRUE),
  # git (legacy)
  list.files(BAG_data_dir_Git,
    pattern = "*FOPH_COVID19_data_extract.csv",
    full.names = TRUE,
    recursive = TRUE))

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]
### Load file data
cat("reading file", newestFile, "...\n")
data_hospitalization <- read.csv(
  newestFile,
  sep = ";", stringsAsFactors = F, header = T)

### Boundaries for curating dates

right_truncation_consolidation <- 1

max_date <- date(max(bagFileDates)) - right_truncation_consolidation
min_date <- as.Date("2020-02-01")

max_delay_hosp <- 30
max_delay_confirm <- 30
max_delay_death <- 100

## Basic data curation
first_curation_data_FOPH <- data_hospitalization %>%
  dplyr::select(manifestation_dt, fall_dt, hospdatin, pttoddat) %>%
  filter(!is.na(manifestation_dt)) %>%
  mutate(across(everything(), ymd)) %>%
  mutate(across(everything(), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA))))

restructured_data_FOPH <- first_curation_data_FOPH %>%
  pivot_longer(cols = c(fall_dt, hospdatin, pttoddat),
               values_to = "count_date",
               names_to = "data_type",
               values_drop_na = T) %>%
  dplyr::select(data_type, everything()) %>%
  dplyr::rename(onset_date = manifestation_dt) %>%
  mutate(data_type = recode(data_type,
    "pttoddat" = "Deaths",
    "hospdatin" = "Hospitalized patients - admission",
    "fall_dt" = "Confirmed cases"))

final_delay_data_FOPH <- restructured_data_FOPH %>%
  mutate(delay = count_date - onset_date) %>%
  mutate(delay = if_else(
    data_type == "Hospitalized patients - admission" & !between(delay, 0, max_delay_hosp),
    as.difftime("NA"),
    delay)) %>%
  mutate(delay = if_else(
    data_type == "Confirmed cases" & !between(delay, 0, max_delay_confirm),
    as.difftime("NA"),
    delay)) %>%
  mutate(delay = if_else(data_type == "Deaths" & !between(delay, 0, max_delay_death), as.difftime("NA"), delay)) %>%
  filter(!is.na(delay)) %>%
  arrange(data_type, onset_date) %>%
  dplyr::group_by(data_type, onset_date) %>%
  slice_sample(count_date, prop = 1) %>% # shuffle rows with the same date
  ungroup() %>%
  mutate(country = "Switzerland", region = "Switzerland", source = "FOPH")


### Save file
write_csv(final_delay_data_FOPH, path = file.path(outDir, "FOPH_data_delays.csv"))


### Investigate delay between symptom onset and hospitalization
rawDataSymptomsToHospital <- subset(data_hospitalization, hospitalisation == 1)
rawDataSymptomsToHospital <- rawDataSymptomsToHospital[, c("manifestation_dt", "hospdatin")]
rawDataSymptomsToHospital <- rawDataSymptomsToHospital[complete.cases(rawDataSymptomsToHospital), ]

onsetDate <- ymd(rawDataSymptomsToHospital[, 1])
onsetDate[onsetDate > max_date] <- NA
onsetDate[onsetDate < min_date] <- NA

hospDate <- ymd(rawDataSymptomsToHospital[, 2])
hospDate[onsetDate > max_date] <- NA
hospDate[onsetDate < min_date] <- NA

datesSymptoms <- data.frame(startSymptoms = onsetDate, hospDate = hospDate)
datesSymptoms$timeFromOnsetToHosp <- datesSymptoms$hospDate - datesSymptoms$startSymptoms

## basic curation
datesSymptoms$timeFromOnsetToHosp[datesSymptoms$timeFromOnsetToHosp < 0 | datesSymptoms$timeFromOnsetToHosp > 30] <- NA
datesSymptoms <- datesSymptoms[complete.cases(datesSymptoms), ]

cat("Mean Time from onset to Hospitalization:",
  mean(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm = TRUE), "\n") # 6.1 (15/05/20)
cat("s.d. Time from onset to Hospitalization:",
  sd(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm = TRUE), "\n") # 4.7

#### Build incidence time series of symptom onsets

dates_onset <- unique(na.omit(data_hospitalization$manifestation_dt))
allDates_onset <-  seq(max(min(as.Date(dates_onset)), min_date), min(max(as.Date(dates_onset)), max_date), by = "days")
 
incidence_onset <- sapply(allDates_onset, function(x) {
  sum(with(data_hospitalization, manifestation_dt == x & hospitalisation == 1), na.rm = TRUE)})
cumul_onset <- cumsum(incidence_onset)

df <- data.frame(Date = allDates_onset, Incidence = incidence_onset, CH = cumul_onset)
df <- filter(df, Date <= max_date)

write_excel_csv(df, path = file.path(outDir, "Hospital_cases_onsets_CH.csv"), quote = F)


#### Build incidence time series of hospitalization date

allDates_admission <-  seq(min_date, max_date, by = "days")

## if neither admission nor onset date is available, use "eingang_dt" as a proxy for the hospitalization date

partialInfoAdmissions <- subset(data_hospitalization,
  ymd(eingang_dt) %in% allDates_admission &
  !(ymd(hospdatin) %in% allDates_admission) &
  !(ymd(manifestation_dt) %in% allDates_onset) &
  hospitalisation == 1)
fullInfoAdmissions <- subset(data_hospitalization,
  ymd(hospdatin) %in% allDates_admission &
  !(ymd(manifestation_dt) %in% allDates_onset) &
  hospitalisation == 1)

incidence_admission <- sapply(allDates_admission, function(x) {
    sum(with(fullInfoAdmissions,  hospdatin == x), na.rm = T) +
    sum(with(partialInfoAdmissions,  eingang_dt == x), na.rm = T)
  })

cumul_admission <- cumsum(incidence_admission)

df <- data.frame(Date = allDates_admission, Incidence = incidence_admission, CH = cumul_admission)
df <- filter(df, Date <= max_date)

write_excel_csv(df, path = file.path(outDir, "Hospital_cases_admissions_CH.csv"), quote = FALSE)

#### transforming data to use in epiestim analysis

allDates <-  seq(min_date, max_date, by = "days")

## if no admission data is available, use "eingang_dt" as a proxy for the hospitalization date

partialInfoAdmissions <- subset(data_hospitalization,
  ymd(eingang_dt) %in% allDates & !(ymd(hospdatin) %in% allDates) & hospitalisation == 1)
fullInfoAdmissions <- subset(data_hospitalization,
ymd(hospdatin) %in% allDates & hospitalisation == 1)

incidence <- sapply(allDates, function(x) {
  sum(with(fullInfoAdmissions,  hospdatin == x), na.rm = TRUE) +
  sum(with(partialInfoAdmissions,  eingang_dt == x), na.rm = TRUE) })

cumul <- cumsum(incidence)
df <- data.frame(Date = allDates, Incidence = incidence, CH = cumul)

write_excel_csv(df, path = file.path(outDir, "Hospital_cases_CH.csv"), quote = FALSE)

library(tidyverse)

confirmedKtn <- data_hospitalization %>%
  dplyr::group_by(ktn, fall_dt) %>%
  dplyr::count() %>%
  ungroup() %>%
  transmute(
    date = fall_dt,
    region = ktn,
    country = "CH",
    source = "FOPH",
    data_type = "confirmed",
    incidence = n
  ) %>%
  dplyr::group_by(region) %>%
  arrange(region, date)

deathsKtn <- data_hospitalization %>%
  filter(!is.na(pttoddat)) %>%
  dplyr::group_by(ktn, pttoddat) %>%
  dplyr::count() %>%
  ungroup() %>%
  transmute(
    date = pttoddat,
    region = ktn,
    country = "CH",
    source = "FOPH",
    data_type = "deaths",
    incidence = n
  ) %>%
  dplyr::group_by(region) %>%
  arrange(region, date)

allKtn <- bind_rows(confirmedKtn, deathsKtn)

allCH <- allKtn %>%
  ungroup() %>%
  dplyr::group_by(date, data_type) %>%
  summarize(
    region = "CH",
    country = "CH",
    source = "FOPH",
    incidence = sum(incidence))

allBAGdata <- bind_rows(allKtn, allCH) %>%
  ungroup() %>%
  complete(date, region, country, source, data_type) %>%
  mutate(incidence = replace_na(incidence, 0)) %>%
  arrange(country, region, data_type, date) %>%
  dplyr::group_by(country, region, source, data_type) %>%
  mutate(cumul = cumsum(incidence)) %>%
  pivot_longer(incidence:cumul, names_to = "variable", values_to = "value") %>%
  dplyr::select(date, region, country, source, data_type, value, variable) %>%
  filter(date <= max_date) %>% 
  arrange(data_type, variable, region, country, source, date)

write_csv(allBAGdata, path = file.path(outDir, "incidence_data_CH.csv"))
