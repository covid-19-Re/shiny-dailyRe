
library(tidyverse)
source(here::here("app/utils.R"))

pathToCountryData <- here::here("app", "data", "countryData")
countryNames <- read_csv(here::here("app", "data", "continents.csv"), col_types = cols(.default = col_character())) %>%
  dplyr::select(-continent)

allData <- list(caseData = list(), estimates = list())
estimatePlotRanges <- list()
allCountries <- str_match(
  string = list.files(path = pathToCountryData, pattern = ".*-Data", recursive = TRUE),
  pattern = "(.*)-.*")[, 2]

for (iCountry in allCountries) {
  iCountryData <- loadCountryData(iCountry, dataDir = pathToCountryData)
  allData$caseData[[iCountry]] <- iCountryData$caseData
  allData$estimates[[iCountry]] <- iCountryData$estimates
  estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]
}

allData$caseData <- bind_rows(allData$caseData) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3")
allData$estimates <- bind_rows(allData$estimates) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3") %>%
  group_by(countryIso3, data_type) %>%
  filter(
      between(date,
        left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
        right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
    ) %>%
  mutate(data_type = as.character(data_type))

saveRDS(allData, file = here::here("app", "data", "allCountryData.rds"))
