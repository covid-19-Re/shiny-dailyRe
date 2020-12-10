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
outDir <- here::here("app", "data", "CHE")

dir.create(outDir, showWarnings = FALSE)

### Find latest BAG data file

bagFiles <- c(
  # polybox
  list.files(BAG_data_dir,
             pattern = "*FOPH_COVID19_data_extract.rds",
             full.names = TRUE,
             recursive = TRUE),
  # git (legacy)
  list.files(BAG_data_dir_Git,
             pattern = "*FOPH_COVID19_data_extract.rds",
             full.names = TRUE,
             recursive = TRUE))

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

maximum_file_date <- max(bagFileDates)
newestFile <- bagFiles[which(bagFileDates == maximum_file_date)[1]]

### Load file data
cat("reading file", newestFile, "...\n")
data_hospitalization <- readRDS(newestFile) %>% as_tibble()


max_date <- date(maximum_file_date)
max_date_plotting <- date(maximum_file_date)
min_date <- as.Date("2020-02-01")

additionalTruncation <- case_when(
  lubridate::wday(max_date) == 3 ~ 1, # 3 = Tue, exclude Sat,
  lubridate::wday(max_date) == 4 ~ 2, # 4 = Wed, exclude Sun and Sat,
  lubridate::wday(max_date) == 5 ~ 3, # 5 = Thu, exclude Mon, Sun and Sat,
  TRUE ~ 0                                # otherwise don't exclude more days
)

right_truncation <- list()
right_truncation[["Confirmed cases"]] <- 3 + additionalTruncation
right_truncation[["Confirmed cases / tests"]] <- 3 + additionalTruncation
right_truncation[["Hospitalized patients"]] <- 5
right_truncation[["Deaths"]] <- 5

max_delay_hosp <- 30
max_delay_confirm <- 30
max_delay_death <- 100

## Basic data curation
first_curation_data_FOPH <- data_hospitalization %>%
  dplyr::select(manifestation_dt, fall_dt, hospdatin, pttoddat) %>%
  filter(!is.na(manifestation_dt)) %>%
  mutate(across(everything(), ymd)) %>%
  mutate(across(everything(), ~ if_else(between(.x, min_date, max_date_plotting), .x, as.Date(NA))))

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
  slice_sample(prop = 1) %>% # shuffle rows with the same date
  ungroup() %>%
  mutate(country = "Switzerland",
         region = "CHE",
         countryIso3 = "CHE",
         source = "FOPH")

normalized_test_delays <- final_delay_data_FOPH %>% filter(data_type == "Confirmed cases") %>% 
  mutate(data_type = "Confirmed cases / tests")

final_delay_data_FOPH <- bind_rows(final_delay_data_FOPH, normalized_test_delays)

### Save file
readr::write_csv(final_delay_data_FOPH, file = file.path(outDir, "CHE_data_delays.csv"))


# cat("Mean Time from onset to Hospitalization:",
#     mean(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm = TRUE), "\n") # 6.1 (15/05/20)
# cat("s.d. Time from onset to Hospitalization:",
#     sd(as.numeric(datesSymptoms$timeFromOnsetToHosp), na.rm = TRUE), "\n") # 4.7

confirmed_case_data <- data_hospitalization %>%
  filter(!is.na(fall_dt)) %>% 
  dplyr::select(manifestation_dt, fall_dt, ktn, exp_ort) %>%
  mutate(across(c(manifestation_dt, fall_dt), ymd)) %>% 
  mutate(across(c(manifestation_dt, fall_dt), ~ if_else(between(.x, min_date, max_date - right_truncation[["Confirmed cases"]]), .x, as.Date(NA)))) %>%
  filter(!is.na(fall_dt)) %>%
  mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"),
         local_infection = if_else(is.na(exp_ort) | exp_ort != 2, "TRUE", "FALSE"),
         date = if_else(is.na(manifestation_dt), fall_dt, manifestation_dt),
         region = ktn,
         .keep = "none") %>%
  dplyr::group_by(region, date, date_type, local_infection) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
      countryIso3 = "CHE",
      source = "FOPH",
      data_type = "confirmed",
      incidence = n,
      .keep  = "unused"
    ) %>% 
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date - right_truncation[["Confirmed cases"]], by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>% 
  arrange(region, date, date_type)

plotting_confirmed_case_data <-  data_hospitalization %>%
  filter(!is.na(fall_dt)) %>% 
  dplyr::select(fall_dt, ktn) %>%
  mutate(date_type = "report_plotting",
         date = ymd(fall_dt),
         region = ktn,
         .keep = "none") %>% 
  filter(between(date, min_date, max_date_plotting)) %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    local_infection = NA,
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "confirmed",
    incidence = n,
    .keep  = "unused"
  ) %>% 
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date_plotting, by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>% 
  arrange(region, date, date_type)

# death_data <- data_hospitalization %>%
#   filter(!is.na(pttoddat)) %>% 
#   dplyr::select(manifestation_dt, pttoddat, ktn) %>%
#   mutate(across(c(manifestation_dt, pttoddat), ymd)) %>% 
#   mutate(across(c(manifestation_dt, pttoddat), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA)))) %>%
#   filter(!is.na(pttoddat)) %>%
#   mutate(manifestation_dt = if_else(between(pttoddat - manifestation_dt, 0, max_delay_death), manifestation_dt, as.Date(NA))) %>%
#   mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"),
#          date = if_else(is.na(manifestation_dt), pttoddat, manifestation_dt),
#          region = ktn,
#          .keep = "none") %>% 
#   dplyr::group_by(region, date, date_type) %>%
#   dplyr::count() %>%
#   ungroup() %>%
#   dplyr::mutate(
#     countryIso3 = "CHE",
#     source = "FOPH",
#     data_type = "deaths",
#     incidence = n,
#     .keep  = "unused"
#   ) %>% 
#   arrange(region, date, date_type)

## only use death dates
death_data <- data_hospitalization %>%
  filter(!is.na(pttoddat)) %>% 
  dplyr::select(pttoddat, ktn, exp_ort) %>%
  mutate(pttoddat = ymd(pttoddat)) %>% 
  mutate(pttoddat = if_else(between(pttoddat, min_date, max_date - right_truncation[["Deaths"]]), pttoddat, as.Date(NA))) %>%
  filter(!is.na(pttoddat)) %>%
  mutate(date_type = "report",
         local_infection = if_else(is.na(exp_ort) | exp_ort != 2, "TRUE", "FALSE"),
         date = pttoddat,
         region = ktn,
         .keep = "none") %>% 
  dplyr::group_by(region, date, date_type, local_infection) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "deaths",
    incidence = n,
    .keep  = "unused"
  ) %>%
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date - right_truncation[["Deaths"]], by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>%
  arrange(region, date, date_type)

plotting_death_data <- data_hospitalization %>%
  filter(!is.na(pttoddat)) %>% 
  dplyr::select(pttoddat, ktn) %>%
  mutate(pttoddat = ymd(pttoddat)) %>% 
  mutate(pttoddat = if_else(between(pttoddat, min_date, max_date_plotting), pttoddat, as.Date(NA))) %>%
  filter(!is.na(pttoddat)) %>%
  mutate(date_type = "report_plotting",
         date = pttoddat,
         region = ktn,
         .keep = "none") %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    local_infection = NA,
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "deaths",
    incidence = n,
    .keep  = "unused"
  ) %>% 
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date_plotting, by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>%
  arrange(region, date, date_type)

hospital_data <- data_hospitalization %>%
  filter(hospitalisation == 1) %>% 
  dplyr::select(eingang_dt, manifestation_dt, hospdatin, ktn, exp_ort) %>% 
  mutate(across(c(eingang_dt, manifestation_dt, hospdatin), ymd)) %>% 
  mutate(hospdatin = if_else(is.na(hospdatin), eingang_dt, hospdatin)) %>% 
  mutate(hospdatin = if_else(between( hospdatin, min_date, max_date - right_truncation[["Hospitalized patients"]]), hospdatin, as.Date(NA))) %>%
  filter(!(is.na(hospdatin))) %>% 
  mutate(manifestation_dt = if_else(between(hospdatin - manifestation_dt, 0, max_delay_hosp), manifestation_dt, as.Date(NA))) %>% 
  # mutate(date_type = if_else(is.na(manifestation_dt), "report", "onset"), #TODO uncomment if onsets are useful
  mutate(date_type = "report",
         # date = if_else(is.na(manifestation_dt), hospdatin, manifestation_dt),
         date = hospdatin,
         local_infection = if_else(is.na(exp_ort) | exp_ort != 2, "TRUE", "FALSE"),
         region = ktn,
         .keep = "none") %>% 
  filter(!is.na(date)) %>% 
  dplyr::group_by(region, date, date_type, local_infection) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "hospitalized",
    incidence = n,
    .keep  = "unused"
  ) %>%
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date - right_truncation[["Hospitalized patients"]], by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>% 
  arrange(region, date, date_type)

plotting_hospital_data <- data_hospitalization %>%
  filter(hospitalisation == 1) %>% 
  dplyr::select(eingang_dt, hospdatin, ktn) %>% 
  mutate(across(c(eingang_dt, hospdatin), ymd)) %>% 
  mutate(hospdatin = if_else(is.na(hospdatin), eingang_dt, hospdatin)) %>% 
  mutate(hospdatin = if_else(between( hospdatin, min_date, max_date_plotting), hospdatin, as.Date(NA))) %>% 
  filter(!(is.na(hospdatin))) %>% 
  mutate(date_type = "report_plotting",
         date = hospdatin,
         region = ktn,
         .keep = "none") %>% 
  filter(!is.na(date)) %>% 
  dplyr::group_by(region, date, date_type) %>%
  dplyr::count() %>%
  ungroup() %>%
  dplyr::mutate(
    local_infection = NA,
    countryIso3 = "CHE",
    source = "FOPH",
    data_type = "hospitalized",
    incidence = n,
    .keep  = "unused"
  ) %>% 
  dplyr::group_by(region, countryIso3, source, data_type, date_type) %>% 
  complete(date = seq(min(date), max_date_plotting, by = "days"), 
           local_infection,
           region, 
           countryIso3, 
           source, 
           data_type, 
           date_type,
           fill = list(incidence = 0)) %>% 
  ungroup() %>% 
  arrange(region, date, date_type)

allKtn <- bind_rows(confirmed_case_data, hospital_data, death_data)

plotting_allKtn <- bind_rows(plotting_confirmed_case_data, plotting_hospital_data, plotting_death_data)

# summarize for CHE and greater Regions

greaterRegions <- tribble(
    ~greaterRegion,             ~region,
    "grR Lake Geneva Region",       c("VD", "VS", "GE"),
    "grR Espace Mittelland",        c("BE", "FR", "SO", "NE", "JU"),
    "grR Northwestern Switzerland", c("BS", "BL", "AG"),
    "grR Zurich",                   c("ZH"),
    "grR Eastern Switzerland",      c("GL", "SH", "AR", "AI", "SG", "GR", "TG"),
    "grR Central Switzerland",      c("LU", "UR", "SZ", "OW", "NW", "ZG"),
    "grR Ticino",                   c("TI")
  ) %>% unnest(cols = c(region))

allCH <- allKtn %>%
  ungroup() %>%
  dplyr::group_by(date, data_type, date_type, local_infection) %>%
  summarize(
    region = "CHE",
    countryIso3 = "CHE",
    source = "FOPH",
    incidence = sum(incidence),
    .groups = "keep")

allGreaterRegions <- allKtn %>%
  ungroup() %>%
  filter(countryIso3 == "CHE") %>%
  left_join(greaterRegions, by = "region") %>%
  ungroup() %>%
  mutate(region = greaterRegion) %>%
  dplyr::select(-greaterRegion) %>%
  group_by(date, region, data_type, date_type, local_infection) %>%
  dplyr::summarize(
    incidence = sum(incidence),
    countryIso3 = "CHE",
    source = "FOPH",
    .groups = "keep")

plotting_allCH <- plotting_allKtn %>%
  ungroup() %>%
  dplyr::group_by(date, data_type, date_type, local_infection) %>%
  summarize(
    region = "CHE",
    countryIso3 = "CHE",
    source = "FOPH",
    incidence = sum(incidence),
    .groups = "keep")

plotting_allGreaterRegions <- plotting_allKtn %>%
  ungroup() %>%
  filter(countryIso3 == "CHE") %>%
  left_join(greaterRegions, by = "region") %>%
  ungroup() %>%
  mutate(region = greaterRegion) %>%
  dplyr::select(-greaterRegion) %>%
  group_by(date, region, data_type, date_type, local_infection) %>%
  dplyr::summarize(
    incidence = sum(incidence),
    countryIso3 = "CHE",
    source = "FOPH",
    .groups = "keep")


allBAGdata <- bind_rows(
    allKtn, allCH, allGreaterRegions,
    plotting_allKtn, plotting_allCH, plotting_allGreaterRegions
  ) %>%
  ungroup() %>%
  mutate(value = replace_na(incidence, 0), .keep = "unused") %>%
  arrange(countryIso3, region, data_type, date_type, local_infection, date) %>%
  dplyr::group_by(countryIso3, region, source, data_type) %>%
  mutate(countryIso3 = if_else(region == "FL", "LIE", countryIso3))


## tests data
bagFiles <- list.files(here::here("app", "data", "BAG"),
                       pattern = "*Timeseries_tests_akl.csv",
                       full.names = TRUE,
                       recursive = TRUE)

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]

nTestsRegions <- read_delim(file = newestFile, delim = ";",
  col_types = cols(
    ktn = col_character(),
    Datum = col_date(format = ""),
    Altersklasse = col_character(),
    Positive = col_double(),
    Negative = col_double()
  )) %>%
  group_by(ktn, Datum) %>%
  summarise(
    positiveTests = sum(Positive),
    negativeTests = sum(Negative),
    .groups = "drop"
  ) %>%
  mutate(
    date = Datum,
    region = ktn,
    countryIso3 = if_else(region == "FL", "LIE", "CHE")
  ) %>%
  dplyr::select(countryIso3, region, date, positiveTests, negativeTests)

nTestsGreaterRegions <- nTestsRegions %>%
  left_join(greaterRegions, by = "region") %>% 
  ungroup() %>%
  mutate(region = greaterRegion) %>%
  dplyr::select(-greaterRegion) %>%
  group_by(countryIso3, region, date) %>%
  dplyr::summarize(
    positiveTests = sum(positiveTests),
    negativeTests = sum(negativeTests),
    .groups = "keep")

bagFiles <- list.files(here::here("app", "data", "BAG"),
                       pattern = "*Time_series_tests.csv",
                       full.names = TRUE,
                       recursive = TRUE)

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]

nTestsCHE <- read_delim(file = newestFile, delim = ";",
    col_types = cols(
      X1 = col_double(),
      Datum = col_date(format = ""),
      `Positive Tests` = col_double(),
      `Negative Tests` = col_double()
  )) %>%
  transmute(
    countryIso3 = "CHE",
    region = "CHE",
    date = Datum,
    positiveTests = `Positive Tests`,
    negativeTests = `Negative Tests`
  )

nTests <- bind_rows(nTestsRegions, nTestsGreaterRegions, nTestsCHE) %>%
  mutate(
    totalTests = positiveTests + negativeTests,
    testPositivity = positiveTests / totalTests
  )

#TODO remove when imports are integrated

allBAGdata_plotting <- allBAGdata %>% filter(is.na(local_infection))

# confirmed cases / test plotting
allBAGdata_plotting_tests <- allBAGdata_plotting %>%
  ungroup() %>%
  filter(
    data_type == "confirmed"
  ) %>%
  left_join(nTests, by = c("date", "region", "countryIso3")) %>%
  mutate(
    data_type = "Confirmed cases / tests",
    value = value / totalTests
  ) %>%
  filter(!is.na(value))

allBAGdata_plotting <- bind_rows(allBAGdata_plotting, allBAGdata_plotting_tests)

allBAGdata_calculations <- allBAGdata %>% filter(!is.na(local_infection))
allBAGdata_calculations <- allBAGdata_calculations %>%
  group_by(date, region, countryIso3, source, data_type, date_type) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(local_infection = "TRUE") %>%
  arrange(region, data_type, date_type, date)

# confirmed cases / test
allBAGdata_calculations_tests <- allBAGdata_calculations %>%
  ungroup() %>%
  filter(
    data_type == "confirmed"
  ) %>%
  left_join(nTests, by = c("date", "region", "countryIso3")) %>%
  mutate(
    data_type = "Confirmed cases / tests",
    value = value / totalTests * mean(totalTests, na.rm = T)
  ) %>%
  filter(!is.na(value))

allBAGdata_calculations <- bind_rows(allBAGdata_calculations, allBAGdata_calculations_tests)

allBAGdata <- bind_rows(list(allBAGdata_plotting), list(allBAGdata_calculations))
## end of remove

qs::qsave(allBAGdata, file = file.path(outDir, "incidence_data_CHE.qs"))
