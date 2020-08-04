if (interactive()) {
  library("lubridate")
  library("fitdistrplus")
  library(here)
  library(tidyverse)
} else {
  suppressPackageStartupMessages({
    library("lubridate")
    library("fitdistrplus")
    library(here)
    library(tidyverse)
  })
}

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
                            "hospdatin" = "Hospitalized patients",
                            "fall_dt" = "Confirmed cases"))

final_delay_data_FOPH <- restructured_data_FOPH %>%
  mutate(delay = as.integer(count_date - onset_date)) %>%
  mutate(delay = if_else(
    data_type == "Hospitalized patients" & !between(delay, 0, max_delay_hosp),
    as.integer(NA),
    delay)) %>%
  mutate(delay = if_else(
    data_type == "Confirmed cases" & !between(delay, 0, max_delay_confirm),
    as.integer(NA),
    delay)) %>%
  mutate(delay = if_else(data_type == "Deaths" & !between(delay, 0, max_delay_death), as.integer(NA), delay)) %>%
  filter(!is.na(delay)) %>%
  arrange(data_type, onset_date) %>%
  dplyr::group_by(data_type, onset_date) %>%
  slice_sample(count_date, prop = 1) %>% # shuffle rows with the same date
  ungroup() %>%
  mutate(country = "Switzerland", region = "CHE", source = "FOPH")

normalized_test_delays <- final_delay_data_FOPH %>% filter(data_type == "Confirmed cases") %>% 
  mutate(data_type = "Confirmed cases / tests")

final_delay_data_FOPH <- bind_rows(final_delay_data_FOPH, normalized_test_delays)
  
## deconvolve symptom onset time series to get infection dates

source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))
# load parameter
source(here::here("app/otherScripts/2_params_InfectionIncidencePars.R"))
# constant delay distribution
incubation_delay_distribution <- get_vector_constant_waiting_time_distr(
  shape_incubation,
  scale_incubation,
  0,
  0)

convolved_delays <- final_delay_data_FOPH %>%
  rename(date = onset_date) %>% 
  dplyr::select(date) %>% 
  group_by(date) %>%  
  tally(name = "value") %>% 
  complete(date = seq.Date(min(date), max(date), by = "days"),
           fill = list(value = 0)) %>% 
  mutate(source = NA, country = NA, region = NA, data_type = NA)

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

max_date <- date(max(bagFileDates)) - right_truncation_consolidation
min_date <- as.Date("2020-02-01")

max_delay_hosp <- 30
max_delay_confirm <- 30
max_delay_death <- 100


confirmed_case_data <- data_hospitalization %>%
  filter(!is.na(fall_dt)) %>% 
  dplyr::select(manifestation_dt, fall_dt, ktn) %>%
  mutate(across(c(manifestation_dt, fall_dt), ymd)) %>% 
  mutate(across(c(manifestation_dt, fall_dt), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA)))) %>%
  mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"),
         date = if_else(is.na(manifestation_dt), fall_dt, manifestation_dt),
         region = ktn,
         .keep = "none") %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
      countryIso3 = "CHE",
      source = "FOPH",
      data_type = "confirmed",
      incidence = n,
      .keep  = "unused"
    ) %>% 
  arrange(region, date, date_type)

death_data <- data_hospitalization %>%
  filter(!is.na(pttoddat)) %>% 
  dplyr::select(manifestation_dt, pttoddat, ktn) %>%
  mutate(across(c(manifestation_dt, pttoddat), ymd)) %>% 
  mutate(across(c(manifestation_dt, pttoddat), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA)))) %>%
  filter(!is.na(pttoddat)) %>%
  mutate(manifestation_dt = if_else(between(pttoddat - manifestation_dt, 0, max_delay_death), manifestation_dt, as.Date(NA))) %>%
  mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"),
         date = if_else(is.na(manifestation_dt), pttoddat, manifestation_dt),
         region = ktn,
         .keep = "none") %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "deaths",
    incidence = n,
    .keep  = "unused"
  ) %>% 
  arrange(region, date, date_type)

hospital_data <- data_hospitalization %>%
  filter(hospitalisation == 1) %>% 
  dplyr::select(eingang_dt, manifestation_dt, hospdatin, ktn) %>% 
  mutate(across(c(eingang_dt, manifestation_dt, hospdatin), ymd)) %>% 
  mutate(across(c(eingang_dt, manifestation_dt, hospdatin), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA)))) %>% 
  mutate(manifestation_dt = if_else(between(hospdatin - manifestation_dt, 0, max_delay_hosp), manifestation_dt, as.Date(NA))) %>% 
  mutate(hospdatin = if_else(is.na(hospdatin), eingang_dt, hospdatin)) %>% 
  mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"),
         date = if_else(is.na(manifestation_dt), hospdatin, manifestation_dt),
         region = ktn,
         .keep = "none") %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "hospitalized",
    incidence = n,
    .keep  = "unused"
  ) %>% 
  arrange(region, date, date_type)

allKtn <- bind_rows(confirmed_case_data, hospital_data, death_data)

allCH <- allKtn %>%
  ungroup() %>%
  dplyr::group_by(date, data_type, date_type) %>%
  summarize(
    region = "CHE",
    countryIso3 = "CHE",
    source = "FOPH",
    incidence = sum(incidence),
    .groups = "keep")

allBAGdata <- bind_rows(allKtn, allCH) %>%
  ungroup() %>%
  complete(date, countryIso3, region, source, data_type) %>%
  mutate(incidence = replace_na(incidence, 0)) %>%
  arrange(countryIso3, region, data_type, date) %>%
  dplyr::group_by(countryIso3, region, source, data_type) %>%
  mutate(cumul = cumsum(incidence)) %>%
  pivot_longer(c(incidence,cumul), names_to = "variable", values_to = "value") %>%
  dplyr::select(date, region, countryIso3, source, data_type, value, variable) %>%
  filter(date <= max_date) %>% 
  arrange(data_type, variable, region, countryIso3, source, date) %>%
  mutate(countryIso3 = if_else(region == "FL", "LIE", countryIso3))

write_csv(allBAGdata, path = file.path(outDir, "incidence_data_CH.csv"))
