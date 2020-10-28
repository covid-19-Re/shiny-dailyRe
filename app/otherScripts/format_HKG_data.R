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

outDir <- here::here("app", "data", "HKG")

dir.create(outDir, showWarnings = FALSE)

url <- "https://api.data.gov.hk/v2/filter?q=%7B%22resource%22%3A%22http%3A%2F%2Fwww.chp.gov.hk%2Ffiles%2Fmisc%2Fenhanced_sur_covid_19_eng.csv%22%2C%22section%22%3A1%2C%22format%22%3A%22csv%22%7D"

cat("reading Hong Kong linelist data file...\n")

data_Hong_Kong <- try(read_csv(url))
if ("try-error" %in% class(data_Hong_Kong)) {
  warning(str_c("Couldn't read Hong Kong linelist data at ", url))
  return(NULL)
}

# data_Hong_Kong %>% filter(`Date of onset` == "Asymptomatic") %>%  summarize(value = n()) 
unique(data_Hong_Kong$`Case classification*`)
# data_Hong_Kong %>% summarize(value = n())

maximum_file_date <- max(as.Date(data_Hong_Kong$`Report date`, format = "%d/%m/%Y"))
max_date <- date(maximum_file_date)
max_date_plotting <- date(maximum_file_date)
min_date <- as.Date("2020-01-01")

max_delay_confirm <- 30

first_curation_data_Hong_Kong <- data_Hong_Kong %>%
  filter(`Confirmed/probable` == "Confirmed") %>% 
  mutate(`Date of onset` =  as.Date(`Date of onset`, format = "%d/%m/%Y"),
         `Case classification*` = if_else(`Case classification*` == "Imported case", "TRUE", "FALSE")) %>% 
  transmute(count_date = as.Date(`Report date`, format = "%d/%m/%Y"),
            onset_date = `Date of onset`,
            local_infection =  `Case classification*`) %>% 
  mutate(across(c(count_date, onset_date), ~ if_else(between(.x, min_date, max_date_plotting), .x, as.Date(NA)))) %>% 
  mutate(data_type = "Confirmed cases",
         countryIso3 = "HKG",
         region = "HKG",
         source = "HK - DoH")

delay_data_Hong_Kong <- first_curation_data_Hong_Kong %>% 
  mutate(delay = as.integer(count_date - onset_date)) %>%
  mutate(delay = if_else(!between(delay, 0, max_delay_confirm),
                         as.integer(NA),
                         delay)) %>% 
  filter(!is.na(delay)) %>%
  dplyr::select(-local_infection) %>% 
  arrange(data_type, onset_date) %>%
  dplyr::group_by(data_type, onset_date) %>%
  slice_sample(prop = 1) %>% # shuffle rows with the same date
  ungroup()

### Save file
write_csv(delay_data_Hong_Kong, path = file.path(outDir, "HKG_data_delays.csv"))

confirmed_case_data <- first_curation_data_Hong_Kong %>%
  mutate(across(c(count_date, onset_date), ~ if_else(between(.x, min_date, max_date), .x, as.Date(NA)))) %>%
  filter(!is.na(count_date)) %>%
  mutate(data_type = "confirmed",
         date_type = if_else(is.na(onset_date), "report", "onset"),
         date = if_else(is.na(onset_date), count_date, onset_date)) %>% 
  dplyr::select(-c(onset_date,count_date)) %>% 
  dplyr::group_by(region, source, countryIso3, data_type, date, date_type, local_infection) %>%
  dplyr::count() %>%
  ungroup() %>% 
  rename(value = n)

start_date <- min(confirmed_case_data$date)
end_date <- max(confirmed_case_data$date)

confirmed_case_data <- confirmed_case_data %>% 
  dplyr::group_by(region, source, countryIso3, date_type, local_infection) %>% 
  complete(date = seq(start_date, end_date, by = "days"), 
           local_infection,
           region,
           countryIso3,
           source,
           data_type,
           date_type,
           fill = list(value = 0)) %>% 
  ungroup() %>%
  arrange(region, date, date_type)



plotting_confirmed_case_data <- first_curation_data_Hong_Kong %>%
  rename(date = count_date) %>% 
  dplyr::select(-onset_date) %>% 
  mutate(data_type = "confirmed",
         date_type = "report_plotting",
         local_infection = NA) %>%
  filter(between(date, min_date, max_date_plotting)) %>% 
  dplyr::group_by(date, local_infection, data_type, date_type, countryIso3, region, source) %>%
  dplyr::count(name = "value") %>%
  ungroup() %>%
  arrange(region, date, date_type)


#TODO remove when imports are integrated
confirmed_case_data <- confirmed_case_data %>% 
  group_by(date, region, countryIso3, source, data_type, date_type) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  mutate(local_infection = "TRUE") %>% 
  arrange(region, data_type, date_type, date)
## end of remove

HKdata <- rbind(confirmed_case_data, plotting_confirmed_case_data)

write_csv(HKdata, path = file.path(outDir, "incidence_data_HKG.csv"))


