startTime <- Sys.time()
saveRDS(startTime, file = "ScriptStartTime.rds")
cat(paste("###", startTime, "- starting 1_getRawData.R", "\n"))

library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library("reshape")
library("plyr")
library("utils")
library("cbsodataR")
library("tidyverse")
library("here")

source(here::here("app/otherScripts/1_utils_getRawData.R"))

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

dataCHHospitalPath <- here::here("app/data/CH")

dataDir <- here::here("app/data/temp")

##### Pull data

##### Swiss data
CHrawData <- getAllSwissData(pathToHospData = dataCHHospitalPath) %>%
  mutate(
    region = recode(region, "CH" = "Switzerland", "FL" = "Liechtenstein"),
    country = recode(country, "CH" = "Switzerland", "FL" = "Liechtenstein"))
# save data

# pathToCHRawDataSave <- file.path(dataDir, "CH_Raw_data.Rdata")
# save(CHrawData, file = pathToCHRawDataSave)
cat("CH\n")

##### European data
countryList <- c("Austria", "Belgium", "France", "Germany", "Italy",
  "Netherlands", "Spain", "Switzerland", "Sweden", "United Kingdom")


ECDCdataRaw <- getLongECDCData(setdiff(countryList, c("Switzerland")))
 
ECDCdata <- ECDCdataRaw %>%
  filter(!(country %in% c("Netherlands")))

swissExcessDeath <- getExcessDeathCH(startAt = as.Date("2020-02-20"))
cat("Swiss Excess\n")

NLdata <- try(getDataNL(stopAfter = Sys.Date() - 1))
if ("try-error" %in% class(NLdata)) {
  NLdata <- NULL
}
cat("NL\n")

ExcessDeathData <- getExcessDeathHMD()
if (!is.null(ExcessDeathData)) {
  ExcessDeathData <- ExcessDeathData %>%
  filter(country %in% countryList)
  cat("HMD\n")
} else {
  cat("get HMD data failed")
}

# Italy

#ITdata <- getITDataPCM() #WIP

pathToExcessDeathIT <- here::here("../covid19-additionalData/excessDeath/Excess_death_IT.csv")
ITExcessDeath <- getExcessDeathIT(filePath = pathToExcessDeathIT, startAt = as.Date("2020-02-20"))
cat("IT\n")

hospitalDataFR <- getHospitalDataFR()
ExcessDeathFR <- getExcessDeathFR()

hospitalDataBE <- getHospitalDataBE()

# pathToExcessDeathUK <- here::here("../covid19-additionalData/excessDeath/Excess_death_UK.xlsx")
# if (file.exists(pathToExcessDeathUK)) {
#   UKExcessDeath <- getExcessDeathUK(
#       startAt = as.Date("2020-02-20"),
#       path_to_data = pathToExcessDeathUK) %>%
#     filter(data_type %in% c("excess_deaths"))
# } else {
#   UKExcessDeath <- NULL
#   cat("UK Excess Death Data file not found. Ignoring... \n")
# }

EUrawData <- bind_rows(
    ECDCdata,
    swissExcessDeath,
    NLdata,
    ExcessDeathData,
    ITExcessDeath,
    hospitalDataFR,
    ExcessDeathFR,
    hospitalDataBE) %>%
  as_tibble()
cat("Bound\n")
# save data
# pathToEURawDataSave <- file.path(dataDir, "EU_Raw_data.rds")
# saveRDS(EUrawData, file = pathToEURawDataSave)

##### Finished pulling data

pathToRawDataSave <- file.path(dataDir, "Raw_data.rds")

rawDataAll <- bind_rows(CHrawData, EUrawData)

# Filter out all regions that do not reach X cumul cases
thresholdCumulCases <- 500

regionsIncluded <- rawDataAll %>%
  filter(variable == "cumul", data_type == "confirmed") %>%
  group_by(region) %>%
  filter(max(value) >= thresholdCumulCases) %>%
  dplyr::select(region, country) %>%
  distinct()
excludedRegions <- setdiff(unique(rawDataAll$region), regionsIncluded$region)

rawData <- rawDataAll %>%
  filter(region %in% regionsIncluded$region) %>%
  mutate(
    data_type = factor(
      data_type,
      levels = c("confirmed",
                 "hospitalized",
                 "hospitalized_onsets",
                 "hospitalized_admissions",
                 "deaths",
                 "excess_deaths"),
      labels = c("Confirmed cases",
                 "Hospitalized patients",
                 "Hospitalized patients - onset",
                 "Hospitalized patients - admission",
                 "Deaths",
                 "Excess deaths"))) %>%
  dplyr::select(country, region, source, data_type, variable, date, value) %>%
  arrange(country, region, source, data_type, variable, date)

cat(str_c(
  "Discarded ", length(excludedRegions), " regions because threshold of ",
  thresholdCumulCases, " confirmed cases wasn't reached.\n",
  "Discarded regions: ", str_c(excludedRegions, collapse = ", "), "\n"))

saveRDS(rawData, file = pathToRawDataSave)

# figuring out when estimation can start (i.e. on the first day confirmed cases are > 100)

estimateStartDates <- rawData %>%
  filter(
    data_type == "Confirmed cases",
    !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
    variable == "cumul",
    value > 100) %>%
  group_by(region, country, source, data_type, variable) %>%
  top_n(n = -1, value) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  dplyr::select(country, region, estimateStart = date)

# figuring out when estimation ends i.e. applying the delays
delays <- tibble(
  data_type = factor(
    c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths"),
    levels = c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")),
  delay = c(8, 10, 18, 30)
)

estimateDatesDf <- rawData %>%
  filter(
    !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
    variable == "cumul",
    !(is.na(value))
  ) %>%
  group_by(region, country, source, data_type) %>%
  top_n(n = 1, date) %>%
  arrange(country, region) %>%
  left_join(delays, by = "data_type") %>%
  ungroup() %>%
  transmute(
    country = country, region = region, data_type = data_type, estimateEnd = date - delay) %>%
  left_join(estimateStartDates, by = c("country", "region"))

estimatesDates <- list()

for (iCountry in unique(estimateDatesDf$country)) {
  tmpCountry <- filter(estimateDatesDf, country == iCountry)
  for (iRegion in unique(tmpCountry$region)) {
    tmpRegion <- filter(tmpCountry, region == iRegion)
    tmpListEnd <- tmpRegion$estimateEnd
    names(tmpListEnd) <- tmpRegion$data_type
    tmpListStart <- tmpRegion$estimateStart
    names(tmpListStart) <- tmpRegion$data_type
    estimatesDates[[iCountry]][[iRegion]] <- list(start = tmpListStart, end = tmpListEnd)
  }
}

pathToEstimateDates <- file.path(dataDir, "estimate_dates.rds")
saveRDS(estimatesDates, file = pathToEstimateDates)

validEstimates <- estimateDatesDf %>%
  filter(!is.na(estimateStart)) %>%
  dplyr::select(country, region) %>%
  distinct()

pathToValidEstimates <- file.path(dataDir, "valid_estimates.rds")
saveRDS(validEstimates, file = pathToValidEstimates)

pathToCountryListSave <- file.path(dataDir, "countryList.rds")
countryList <- unique(validEstimates$country)
saveRDS(countryList, file = pathToCountryListSave)

pathToLatestData <- file.path(dataDir, "latestData.rds")
  
latestData <- rawData %>%
  mutate(
    data_type = replace(
      data_type,
      data_type %in% c("Hospitalized patients - onset", "Hospitalized patients - admission"),
      "Hospitalized patients")) %>%
  group_by(country, region, source, data_type) %>%
  dplyr::summarize(date = max(date)) %>%
  left_join(tribble(
      ~source, ~sourceLong, ~url,
      #"openZH", "Data for Cantons and the Principality of Liechtenstein, aggregated by the statistical office of the canton Zürich", "https://github.com/openZH/covid_19/",
      "FOPH",   "Data from the Swiss Federal Office of Public Health", "",
      "BFS",    "Data from the Swiss Federal Office of Statistics", "https://www.bfs.admin.ch/bfsstatic/dam/assets/12727505/master",
      "RIVM",   "Data from the Dutch National Institute for Public Health and the Environment", "https://github.com/J535D165/CoronaWatchNL",
      "CBS",    "Data from the Dutch Central Office for Statistics", "https://opendata.cbs.nl/statline/#/CBS/nl/dataset/70895ned/table?fromstatweb",
      "ONS",    "Data from the UK Office of National Statistics (England and Wales)", "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales",
      "ECDC",   "Data from the European Center for Disease Prevention and Control", "https://opendata.ecdc.europa.eu/covid19/casedistribution/",
      "HMD",    "Data from the Human Mortality Database - Short-term Mortality Fluctuations Data Series", "https://www.mortality.org/",
      "Istat",  "Data from the Italian National Institute of Statistics", "https://www.istat.it/it/archivio/240401",
      "SpF-DMI", "Data from the French National Public Health Agency", "https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
      "Economist", "Data from the Economist Excess Death Tracker", "https://github.com/TheEconomist/covid-19-excess-deaths-tracker",
      "Sciensano", "Data from the Belgian Institute for Health", "https://epistat.wiv-isp.be/covid/"
    ), by = "source")

saveRDS(latestData, file = pathToLatestData)

# population sizes for plotting
pathToPopSizes <- file.path(dataDir, "popSizes.Rdata")

popSizesECDC <- ECDCdataRaw %>%
  group_by(country, region) %>%
  summarize(popSize = popSize[1])

popSizesCH <- read_csv(here("app/data/popSizesCHFL.csv"),
  col_types = cols(
    country = col_character(),
    region = col_character(),
    popSize = col_double()
  )
)

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

popSizesCHgrR <- popSizesCH %>%
  filter(country == "Switzerland", region != "Switzerland") %>%
  left_join(greaterRegions, by = "region") %>%
  ungroup() %>%
  mutate(region = greaterRegion) %>%
  dplyr::select(-greaterRegion) %>%
  group_by(country, region) %>%
  dplyr::summarize(
    popSize = sum(popSize))

popSizes <- bind_rows(popSizesECDC, popSizesCH, popSizesCHgrR)
save(popSizes, file = pathToPopSizes)

cat(paste("###", Sys.time(), "- done 1_getRawData.R", "\n"))
