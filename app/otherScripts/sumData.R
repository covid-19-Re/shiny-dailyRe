library(tidyverse)
cat(str_c(Sys.time(), " | summarizing data ...\n"))
source(here::here("app/utils.R"))

pathToCountryData <- here::here("app", "data", "countryData")
countryNames <- read_csv(here::here("app", "data", "continents.csv"), col_types = cols(.default = col_character())) %>%
  dplyr::select(-continent)

allCountries <- str_match(
  string = list.files(path = pathToCountryData, pattern = ".*-Data", recursive = TRUE),
  pattern = "(.*)-.*")[, 2]

allData <- list(caseData = list(), estimates = list())
estimatePlotRanges <- list()
pb <- txtProgressBar(min = 0, max = length(allCountries))
pb_i <- 0
for (iCountry in allCountries) {
  iCountryData <- loadCountryData(iCountry, dataDir = pathToCountryData)
  allData$caseData[[iCountry]] <- iCountryData$caseData
  allData$estimates[[iCountry]] <- iCountryData$estimates
  estimatePlotRanges[[iCountry]] <- iCountryData$estimateRanges[[iCountry]]
  pb_i <- pb_i + 1
  setTxtProgressBar(pb, pb_i)
}
close(pb)
allData$caseData <- bind_rows(allData$caseData) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3")
allData$estimates <- bind_rows(allData$estimates) %>%
  # fix country names
  dplyr::select(-country) %>%
  left_join(countryNames, by = "countryIso3") %>%
  # remove stringency index if it exists
  filter(data_type != "Stringency Index") %>%
  group_by(countryIso3, data_type) %>%
  filter(
      between(date,
        left = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["start"]][[as.character(data_type[1])]],
        right = estimatePlotRanges[[countryIso3[1]]][[countryIso3[1]]][["end"]][[as.character(data_type[1])]])
    ) %>%
  mutate(data_type = as.character(data_type))

saveRDS(allData, file = here::here("app", "data", "allCountryData.rds"))

# update updateData
updateData <- readRDS(here::here("app", "data", "temp", "updateDataTemp.rds"))
saveRDS(updateData, here::here("app", "data", "updateData.rds"))
cat(str_c(Sys.time(), " | summarizing data done.\n"))
