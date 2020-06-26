library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
#library("gdata")
library("fitdistrplus")
library(here)

BAG_data_dir <- here("app", "data", "BAG")
BAG_data_dir_Git <- here::here("../ch-hospital-data/data/CH")
# output_data_dir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data"
outDir <- here("app","data", "CH")
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
dataHospitalization <- read.csv(
  newestFile,
  sep = ";", stringsAsFactors = F, header = T)

### Boundaries for curating dates
maxDate <- date(max(bagFileDates))
minDate <- as.Date("2020-02-01")

### Construct file with recorded delays between symptom onset and test report
dataOnsetToReport <- dataHospitalization[,c("manifestation_dt", "fall_dt", "hospdatin")]
# dataOnsetToReport <- dataOnsetToReport[complete.cases(dataOnsetToReport),]

## basic data curation

maxDelay <- 30

hospDate <- ymd(dataOnsetToReport[,3])
hospDate[hospDate > maxDate] <- NA
hospDate[hospDate < minDate] <- NA

onsetDate <- ymd(dataOnsetToReport[,1])
onsetDate[onsetDate > maxDate] <- NA
onsetDate[onsetDate < minDate] <- NA

testDate<- ymd(dataOnsetToReport[,2])
testDate[testDate > maxDate] <- NA
testDate[testDate < minDate] <- NA

onsetToTest <- testDate - onsetDate
onsetToTest[onsetToTest < 0 ] <- NA
onsetToTest[onsetToTest >  maxDelay] <- NA

onsetToHosp <- hospDate - onsetDate
onsetToHosp[onsetToHosp < 0 ] <- NA
onsetToHosp[onsetToHosp >  maxDelay] <- NA

testDate <- testDate[!is.na(onsetToTest)]
onsetToTest <- onsetToTest[!is.na(onsetToTest)]

hospDate <- hospDate[!is.na(onsetToHosp)]
onsetToHosp <- onsetToHosp[!is.na(onsetToHosp)]

df_test <- data.frame(date=testDate, delay=onsetToTest, data_type="Confirmed cases", region="Switzerland")
df_hosp <- data.frame(date=hospDate, delay=onsetToHosp, data_type="Hospitalized patients - admission", region="Switzerland")

df <- rbind(df_test, df_hosp)
df <- df[sample(1:nrow(df)),] # shuffle first to remove potential biases
df <- df[order(df$date),]

### Save file
# write_csv(df, path = file.path(output_data_dir,"FOPH_data_delays.csv"))
write_csv(df, path = file.path(outDir,"FOPH_data_delays.csv"))


### Investigate delay between symptom onset and hospitalization
rawDataSymptomsToHospital <- subset(dataHospitalization, hospitalisation == 1) 
rawDataSymptomsToHospital <- rawDataSymptomsToHospital[,c("manifestation_dt","hospdatin")]
rawDataSymptomsToHospital <- rawDataSymptomsToHospital[complete.cases(rawDataSymptomsToHospital),]

onsetDate <- ymd(rawDataSymptomsToHospital[,1])
onsetDate[onsetDate > maxDate] <- NA
onsetDate[onsetDate < minDate] <- NA

hospDate<- ymd(rawDataSymptomsToHospital[,2])
hospDate[onsetDate > maxDate] <- NA
hospDate[onsetDate < minDate] <- NA

datesSymptoms <- data.frame(startSymptoms=onsetDate, hospDate=hospDate)
datesSymptoms$timeFromOnsetToHosp <- datesSymptoms$hospDate - datesSymptoms$startSymptoms

## basic curation
datesSymptoms$timeFromOnsetToHosp[datesSymptoms$timeFromOnsetToHosp < 0 | datesSymptoms$timeFromOnsetToHosp > 30] <- NA
datesSymptoms <- datesSymptoms[complete.cases(datesSymptoms), ]

cat("Mean Time from onset to Hospitalization:",
  mean(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm=T), "\n") # 6.1 (15/05/20)
cat("s.d. Time from onset to Hospitalization:",
  sd(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm=T), "\n") # 4.7

#### Build incidence time series of symptom onsets

dates_onset <- unique(na.omit(dataHospitalization$manifestation_dt))
allDates_onset <-  seq(max(min(as.Date(dates_onset)), minDate), min(max(as.Date(dates_onset)), maxDate), by="days")
 
incidence_onset <- sapply(allDates_onset, function(x) { sum(with(dataHospitalization, manifestation_dt == x & hospitalisation == 1), na.rm = T)})
cumul_onset <- cumsum(incidence_onset)

df <- data.frame(Date=allDates_onset, Incidence=incidence_onset, CH=cumul_onset)

#write_excel_csv(df, path=file.path(output_data_dir, paste0("Hospital_cases_onsets_CH_", format(Sys.Date(), "%y%m%d"), ".csv")), quote=F)
#write_excel_csv(df, path=file.path(output_data_dir, "Hospital_cases_onsets_CH.csv"), quote=F) 
write_excel_csv(df, path=file.path(outDir, "Hospital_cases_onsets_CH.csv"), quote=F) 


#### Build incidence time series of hospitalization date

allDates_admission <-  seq(minDate, maxDate, by="days")

## if neither admission nor onset date is available, use "eingang_dt" as a proxy for the hospitalization date

partialInfoAdmissions <- subset(dataHospitalization, ymd(eingang_dt) %in% allDates_admission & !(ymd(hospdatin) %in% allDates_admission) & !(ymd(manifestation_dt) %in% allDates_onset) & hospitalisation == 1)
fullInfoAdmissions <- subset(dataHospitalization, ymd(hospdatin) %in% allDates_admission & !(ymd(manifestation_dt) %in% allDates_onset) & hospitalisation == 1)

incidence_admission <- sapply(allDates_admission, function(x) { sum(with(fullInfoAdmissions,  hospdatin == x), na.rm = T) + sum(with(partialInfoAdmissions,  eingang_dt == x), na.rm = T) })

cumul_admission <- cumsum(incidence_admission)

df <- data.frame(Date=allDates_admission, Incidence=incidence_admission, CH=cumul_admission)

#write_excel_csv(df, path=file.path(output_data_dir, paste0("Hospital_cases_admissions_CH_", format(Sys.Date(), "%y%m%d"), ".csv")), quote=F)
#write_excel_csv(df, path=file.path(output_data_dir, "Hospital_cases_admissions_CH.csv"), quote=F) 
write_excel_csv(df, path=file.path(outDir, "Hospital_cases_admissions_CH.csv"), quote=F) 

#### transforming data to use in epiestim analysis

allDates <-  seq(minDate, maxDate, by="days")

## if no admission data is available, use "eingang_dt" as a proxy for the hospitalization date

partialInfoAdmissions <- subset(dataHospitalization, ymd(eingang_dt) %in% allDates & !(ymd(hospdatin) %in% allDates) & hospitalisation == 1)
fullInfoAdmissions <- subset(dataHospitalization, ymd(hospdatin) %in% allDates & hospitalisation == 1)

incidence <- sapply(allDates, function(x) { sum(with(fullInfoAdmissions,  hospdatin == x), na.rm = T) + sum(with(partialInfoAdmissions,  eingang_dt == x), na.rm = T) })

cumul <- cumsum(incidence)
df <- data.frame(Date=allDates, Incidence=incidence, CH=cumul)


#write_excel_csv(df, path=file.path(output_data_dir, paste0("Hospital_cases_CH_", format(Sys.Date(), "%y%m%d"), ".csv")), quote=F)
#write_excel_csv(df, path=file.path(output_data_dir, "Hospital_cases_CH.csv"), quote=F) 
write_excel_csv(df, path=file.path(outDir, "Hospital_cases_CH.csv"), quote=F) 

library(tidyverse)

confirmedKtn <- dataHospitalization %>%
  group_by(ktn, fall_dt) %>%
  count() %>%
  ungroup() %>%
  transmute(
    date = fall_dt,
    region = ktn,
    country = "CH",
    source = "FOPH",
    data_type = "confirmed",
    incidence = n
  ) %>%
  group_by(region) %>%
  arrange(region, date)

deathsKtn <- dataHospitalization %>%
  filter(!is.na(pttoddat)) %>%
  group_by(ktn, pttoddat) %>%
  count() %>%
  ungroup() %>%
  transmute(
    date = pttoddat,
    region = ktn,
    country = "CH",
    source = "FOPH",
    data_type = "deaths",
    incidence = n
  ) %>%
  group_by(region) %>%
  arrange(region, date)

allKtn <- bind_rows(confirmedKtn, deathsKtn)

allCH <- allKtn %>%
  ungroup() %>%
  group_by(date, data_type) %>%
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
  group_by(country, region, source, data_type) %>%
  mutate(cumul = cumsum(incidence)) %>%
  pivot_longer(incidence:cumul, names_to = "variable", values_to = "value") %>%
  dplyr::select(date, region, country, source, data_type, value, variable) %>%
  arrange(data_type, variable, region, country, source, date)

write_csv(allBAGdata, path = file.path(outDir, "incidence_data_CH.csv"))
