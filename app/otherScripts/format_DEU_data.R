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

outDir <- here::here("app", "data", "DEU")

dir.create(outDir, showWarnings = FALSE)

url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"

cat("reading German linelist data file...\n")

data_Germany <- try(read_csv(url))
if ("try-error" %in% class(data_Germany)) {
  warning(str_c("Couldn't read Germany linelist data at ", url))
  return(NULL)
}

parseRKIdate <- function(x) {
  as_date(parse_date_time(x, "%Y/%m/%d %H%M%S", tz = "UTC"))
}

reformatted_DEU_data <- data_Germany %>%
  mutate(
    count_date = parseRKIdate(Meldedatum),
    reference_date = parseRKIdate(Refdatum),
    symptom_onset = IstErkrankungsbeginn
  ) %>%
  filter(NeuerFall %in% c(0, 1)) %>%
  dplyr::select(c(
    "reference_date", "count_date", "symptom_onset",
    "AnzahlFall"
  )) %>%
  uncount(AnzahlFall) %>%
  arrange(reference_date)

right_truncation <- list()
right_truncation[["Confirmed cases"]] <- 3

maximum_file_date <- max(reformatted_DEU_data$count_date)
max_date <- date(maximum_file_date)
max_date_plotting <- date(maximum_file_date)
min_date <- as.Date("2020-01-01")

max_delay_confirm <- 30

first_curation_DEU_data <- reformatted_DEU_data %>%
  mutate(across(c(count_date, reference_date), ~ if_else(between(.x, min_date, max_date_plotting), .x, as.Date(NA)))) %>%
  filter(!is.na(count_date)) %>%
  mutate(reference_date = if_else(symptom_onset == 1, reference_date, as.Date(NA))) %>%
  dplyr::select(-symptom_onset) %>%
  rename(onset_date = reference_date) %>%
  mutate(onset_date = if_else(between((count_date - onset_date), 0, max_delay_confirm), onset_date, as.Date(NA))) %>%
  mutate(
    data_type = "Confirmed cases",
    countryIso3 = "DEU",
    country = "Germany",
    region = "DEU",
    source = "RKI"
  )

delays_data_DEU <- first_curation_DEU_data %>%
  mutate(delay = as.integer(count_date - onset_date)) %>%
  filter(!is.na(delay)) %>%
  arrange(data_type, onset_date) %>%
  dplyr::group_by(data_type, onset_date) %>%
  slice_sample(prop = 1) %>% # shuffle rows with the same date
  ungroup()

write_csv(delays_data_DEU, file = file.path(outDir, "DEU_data_delays.csv"))


confirmed_case_data <- first_curation_DEU_data %>%
  mutate(
    across(
      c(count_date, onset_date),
      ~ if_else(between(.x, min_date, max_date - right_truncation[["Confirmed cases"]]), .x, as.Date(NA))
  )) %>%
  filter(!is.na(count_date)) %>%
  mutate(
    data_type = "confirmed",
    local_infection = "TRUE",
    date_type = if_else(is.na(onset_date), "report", "onset"),
    date = if_else(is.na(onset_date), count_date, onset_date)
  ) %>%
  dplyr::select(-c(onset_date, count_date)) %>%
  dplyr::group_by(region, source, countryIso3, data_type, date, date_type, local_infection) %>%
  dplyr::count() %>%
  ungroup() %>%
  rename(value = n)

start_date <- min(confirmed_case_data$date)
end_date <- max(confirmed_case_data$date)

confirmed_case_data <- confirmed_case_data %>%
  dplyr::group_by(region, source, countryIso3, date_type, local_infection) %>%
  complete(
    date = seq(start_date, end_date, by = "days"),
    data_type,
    fill = list(value = 0)
  ) %>%
  ungroup() %>%
  arrange(region, date, date_type)

plotting_confirmed_case_data <- first_curation_DEU_data %>%
  rename(date = count_date) %>%
  dplyr::select(-onset_date) %>%
  mutate(
    data_type = "confirmed",
    date_type = "report_plotting",
    local_infection = NA
  ) %>%
  filter(between(date, min_date, max_date_plotting)) %>%
  dplyr::group_by(date, local_infection, data_type, date_type, countryIso3, region, source) %>%
  dplyr::count(name = "value") %>%
  ungroup() %>%
  arrange(region, date, date_type)


# TODO remove when imports are integrated
confirmed_case_data <- confirmed_case_data %>%
  group_by(date, region, countryIso3, source, data_type, date_type) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(local_infection = "TRUE") %>%
  arrange(region, data_type, date_type, date)
## end of remove

DEU_data <- rbind(confirmed_case_data, plotting_confirmed_case_data)

write_csv(DEU_data, path = file.path(outDir, "incidence_data_DEU.csv"))
