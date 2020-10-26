library("lubridate")
library("readr")
library("gridExtra")
#library("gdata")
library("fitdistrplus")
library(here)
library(tidyverse)
library(zoo)

BAG_data_dir <- here::here("app", "data", "BAG")
# output_data_dir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data"
outDir <- here::here("app","data", "CH")
dir.create(outDir, showWarnings = FALSE)

### Find latest BAG data file

bagFiles <- list.files(BAG_data_dir,
    pattern = "*FOPH_COVID19_data_extract.csv",
    full.names = TRUE,
    recursive = TRUE)

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

recent_dates <- unique(sort(bagFileDates, decreasing = T))
# recent_dates <- unique(sort(bagFileDates, decreasing = T))[1]

i <- 1

for(i in 1:length(recent_dates) ) {
  newestFile <- bagFiles[which(bagFileDates == recent_dates[i])[1]]
  
  ### Load file data
  cat("reading file", newestFile, "...\n")
  data_hospitalization <- read.csv(
    newestFile,
    sep = ";", stringsAsFactors = F, header = T)
  
  ### Boundaries for curating dates
  
  truncation <- 0
  max_date <- date(recent_dates[i]) - truncation
  min_date <- as.Date("2020-02-01")
  
  max_delay_hosp <- 30
  max_delay_confirm <- 30
  max_delay_death <- 100
  
  ## Basic data curation
  first_curation_data_FOPH <- data_hospitalization %>% 
    dplyr::select(manifestation_dt, fall_dt, hospdatin, pttoddat) %>%
    filter(!is.na(manifestation_dt)) %>%
    mutate(across(everything(), ymd)) %>%
    mutate(across(everything(), ~ if_else( between( .x, min_date, max_date), .x, as.Date(NA))))
  
  restructured_data_FOPH <- first_curation_data_FOPH %>% 
    pivot_longer(cols = c(fall_dt, hospdatin, pttoddat),
                 values_to = "count_date",
                 names_to = "data_type",
                 values_drop_na = T) %>%
    dplyr::select(data_type, everything()) %>%
    dplyr::rename(onset_date = manifestation_dt) %>%
    mutate(data_type = recode(data_type,
                              "pttoddat"="Deaths",
                              "hospdatin" = "Hospitalized patients - admission",
                              "fall_dt" = "Confirmed cases"))
  
  final_delay_data_FOPH <- restructured_data_FOPH %>%
    mutate(delay = as.Date(count_date) - as.Date(onset_date)) %>%
    mutate(delay = if_else(data_type == "Hospitalized patients - admission" & !between(delay, 0, max_delay_hosp), as.difftime("NA"), delay)) %>%
    mutate(delay = if_else(data_type == "Confirmed cases" & !between(delay, 0, max_delay_confirm), as.difftime("NA"), delay)) %>%
    mutate(delay = if_else(data_type == "Deaths" & !between(delay, 0, max_delay_death), as.difftime("NA"), delay)) %>%
    filter(!is.na(delay)) %>%
    arrange(data_type, onset_date) %>%
    dplyr::group_by(data_type, onset_date) %>%
    slice_sample(count_date, prop = 1) %>% # shuffle rows with the same date
    ungroup() %>%
    mutate(country = "Switzerland", region = "Switzerland", source = "FOPH")
  
  
  ### Save file
  write_csv(final_delay_data_FOPH, path=file.path(outDir, paste0("FOPH_data_delays_",format(recent_dates[i], "%Y-%m-%d"), ".csv")))
  
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
    dplyr::summarize(
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
  
  write_csv(allBAGdata, path = file.path(outDir, paste0("incidence_data_CH_", format(recent_dates[i], "%Y-%m-%d"), ".csv")))
  
}
