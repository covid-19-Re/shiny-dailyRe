#TODO remove
loadCountryData <- function(iso3, dataDir = "data/countryData") {
  
  allPaths <- list.files(path = dataDir, recursive = TRUE)
  
  dataPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-Data.rds"))
  if (!is_empty(dataPath)){
    caseData <- readRDS(file.path(dataDir, dataPath))
    if (nrow(caseData %>% filter(date_type == "report_plotting")) > 0) {
      caseData <- caseData %>%
        filter(date_type == "report_plotting")
    } else {
      caseData <- caseData %>%
        dplyr::group_by(date, region, country, countryIso3, source, data_type, populationSize) %>%
        # there should only be one "date_type" but the summing is left in there in case.
        dplyr::summarise(value = sum(value), .groups = "drop")
    }
  } else {
    caseData <- NULL
  }
  
  deconvolutedDataPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-DeconvolutedData.rds"))
  print(deconvolutedDataPath)
  if (!is_empty(deconvolutedDataPath)) {
    deconvolutedData <- readRDS(file.path(dataDir, deconvolutedDataPath)) %>%
      mutate(data_type = str_sub(data_type, 11)) %>%
      group_by(date, region, country, source, data_type) %>%
      summarise(
        deconvoluted = mean(value),
        deconvolutedLow = deconvoluted - sd(value),
        deconvolutedHigh = deconvoluted + sd(value),
        .groups = "keep"
      )
    caseData <- caseData %>%
      left_join(deconvolutedData, by = c("country", "region", "source", "data_type", "date")) %>%
      arrange(countryIso3, region, source, data_type, date)
  }
  
  estimatesPath <- str_subset(string = allPaths, pattern = str_c(iso3, "-Estimates.rds"))
  if (!is_empty(estimatesPath)) {
    estimates <- readRDS(file.path(dataDir, estimatesPath))
  } else {
    estimates <- NULL
  }
  
  if (!is.null(caseData)) {
    estimateRanges <- estimateRanges(
      caseData,
      minConfirmedCases = 100,
      delays = delaysDf)
  } else (
    estimateRanges <- NULL
  )
  
  
  countryData <- list(
    caseData = caseData,
    estimates = estimates,
    estimateRanges = estimateRanges)
  
  return(countryData)
}

library(tidyverse)
# source(here::here("app/utils.R")) #TODO undo comment

pathToCountryData <- here::here("app", "data", "countryData")

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
