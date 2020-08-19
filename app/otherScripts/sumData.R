
library(tidyverse)
source(here::here("app/utils.R"))

pathToCountryData <- here::here("app", "data", "countryData")

allData <- list(caseData = list(), estimates = list())
estimatePlotRanges <- list()
allCountries <- str_match(
  string = list.files(path = pathToCountryData, pattern = ".*-Data", recursive = TRUE),
  pattern = "(.*)-.*")[, 2]
for (iCountry in allCountries) {
  iCountryData <- loadCountryData(iCountry, dataDir = here::here("app", "data", "countryData"))
  allData$caseData[[iCountry]] <- iCountryData$caseData
  allData$estimates[[iCountry]] <- iCountryData$estimates
  estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]
}

allData$caseData <- bind_rows(allData$caseData)
allData$estimates <- bind_rows(allData$estimates) %>%
  group_by(countryIso3, data_type) %>%
  filter(
      between(date,
        left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
        right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
    ) %>%
  mutate(data_type = as.character(data_type))

saveRDS(allData, file = here::here("app", "data", "allCountryData.rds"))
