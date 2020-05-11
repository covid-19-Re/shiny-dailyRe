print(paste("starting 1_getRawData.R:", Sys.time()))

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

cantonList <- c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH")
countryList <- c("Austria", "Belgium", "France", "Germany", "Italy",
  "Netherlands", "Spain", "Switzerland", "Sweden", "United Kingdom")

###############################################
################ Utilities ####################
###############################################

## Get incidence data from cumulative counts data and restructure dataframe to long format
## Any data after "stoppingDate" is excluded
meltCumulativeData <- function(
  rawData,
  dataType,
  dataSource,
  country="CH",
  nameDateCol="Date",
  stoppingDate = (Sys.Date() - 1)) {

  cumulData <- rawData
  cumulData$Date <- ymd(cumulData$Date, locale = "en_GB.UTF-8")
  cumulData <- cumulData[cumulData$Date <= stoppingDate, ]
  cumulData <- melt(cumulData, id.vars = nameDateCol)
  colnames(cumulData) <- c("date", "region", "value")

  cumulData <- rbind.fill(lapply(
    unique(cumulData$region),
    function(reg) {
      curateLongTimeSeries(subset(cumulData, region == reg), isIncidenceData = F)
    }
  ))

  cumulData$data_type <- dataType
  cumulData$variable <- "cumul"
  cumulData$country <- country
  cumulData$source <- dataSource
  cumulData <- cumulData[, c("date", "region", "country", "source", "data_type", "value", "variable")]

  incidenceData <- rbind.fill(lapply(
    unique(cumulData$region), function(reg) {
      incidenceSeries <- subset(cumulData, region == reg);
      incidence <- diff(incidenceSeries$value);
      incidenceSeries <- incidenceSeries[-1, ]
      incidenceSeries$value <- incidence
      return(incidenceSeries);
    }
  ))
  incidenceData$variable <- rep("incidence", nrow(incidenceData))

  return(rbind(cumulData, incidenceData))
}

## Prepare time series to be compatible with EpiEstim
curateLongTimeSeries <- function(data, isIncidenceData = TRUE) {
  ## Remove missing data at beginning of series
  while (nrow(data) > 0 & is.na(data$value[1])) {
    data <- data[-1, ]
    if (nrow(data) == 0) {
      return(data.frame())
    }
  }

  ## Remove missing data at the end of the series
  while (nrow(data) > 0 & is.na(data$value[nrow(data)])) {
    data <- data[-nrow(data), ]
    if (nrow(data) == 0) {
      return(data.frame())
    }
  }

  if (isIncidenceData == TRUE) { # incidence time series
    ## Replace missing data in rest of series by zeroes (required for using EpiEstim)
    data[is.na(data$value), "value"] <- 0
  } else { # cumulative counts time series
    ## Replace missing values by the previous day"s value
    for (i in 2:nrow(data)) {
      if (is.na(data[i, "value"])) {
        data[i, "value"] <- data[i - 1, "value"]
      }
    }
  }
  return(data)
}

getCountryData <- function(countries, data = getDataECDC()) {
  subset_data <- data %>%
    filter(country %in% countries)
  return(subset_data)
}

getCumulData <- function(data) {
  cumulData <- data %>%
    group_by(country, data_type) %>%
    arrange(date) %>%
    mutate(value = cumsum(value), variable = "cumul") %>%
    arrange(country)
  return(cumulData)
}

########################################
############ Data fetching #############
########################################

##### Swiss Data #######
## Fetch data from openZH via Daniel Probst"s repo
getSwissDataFromOpenZH <- function(stopAfter = (Sys.Date() - 1)) {

  countTypes <- list("confirmed", "deaths")
  typeLabels <- list("cases", "fatalities")
  names(countTypes) <- typeLabels
  names(typeLabels) <- countTypes

  baseUrl <-  "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/"

  data <- data.frame(
    date = c(),
    region = c(),
    country = c(),
    value = c(),
    data_type = c(),
    variable = c(),
    estimate_type = c())

  for (typeLabel in typeLabels) {
    cumulFileUrl <- paste0(baseUrl, "covid19_", typeLabel, "_switzerland_openzh.csv")
    # cumulFileUrl <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/openZH_daenuprobst/covid19_cases_switzerland_openzh.csv" # Fix while raw.github is down
    cumulData <- read.csv(cumulFileUrl)
    data <- rbind(
      data,
      meltCumulativeData(cumulData,
        dataType = countTypes[[typeLabel]],
        dataSource = "openZH",
        country = "CH",
        stoppingDate = stopAfter))
  }
  return(data)
}

## Include hospitalization counts from local csv files
getHospitalData <- function(path, region = "CH", csvBaseName="Hospital_cases_") {
  filePath <- file.path(path, str_c(csvBaseName, region, ".csv"))
  cumData <- read_csv(filePath,
    col_types = cols(
      Date = col_date(format = ""),
      Incidence = col_double(),
      CH = col_double()))
  cumData <- cumData[, c(1, 3)]
  out <- meltCumulativeData(cumData,
    dataType = "hospitalized",
    country = "CH",
    dataSource = "FOPH")
  return(out)
}

## Combine openZH data with hospitalization data
getAllSwissData <- function(stoppingAfter = (Sys.Date() - 1), pathToHospData, regions = cantonList) {
  openZHData <- getSwissDataFromOpenZH(stopAfter = stoppingAfter)
  hospitalData <- rbind(getHospitalData(path = pathToHospData, region = "CH"))
  swissData <- subset(rbind(openZHData, hospitalData), region %in% regions)
  return(swissData)
}

getExcessDeathCH <- function(startAt = as.Date("2020-02-20")) {
  # urlPastData = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12607335/master"
  # pastData <- read_delim(urlPastData, delim = ";", comment = "#")

  url2020 <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/12727505/master"
  data2020 <- read_delim(url2020, delim = ";")

  relevant_weeks <- seq(isoweek(startAt), isoweek(Sys.Date()) - 2)

  tidy_data <- data2020 %>%
    select(date = Ending, week = Week, age = Age,
           avg_deaths = Expected, deaths = extrapol) %>%
    filter(week %in% relevant_weeks) %>%
    mutate(date = dmy(date))

  longData <- tidy_data %>%
    group_by(date) %>%
    summarise_at(vars(deaths, avg_deaths), list(total = sum)) %>%
    mutate(excess_deaths = deaths_total - avg_deaths_total) %>%
    select(date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(country = "Switzerland", variable = "incidence",
           region = country, source = "BFS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  cumulData <- getCumulData(longData)
  longData <- rbind.fill(longData, cumulData)

  return(longData)
}

##### Dutch Data ##########################################
getDataNL <- function(stopAfter = Sys.Date(), startAt = as.Date("2020-02-20")) {
  baseurl <- "https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_national_by_date/rivm_NL_covid19_national_by_date_"
  urlfile <- paste0(baseurl, stopAfter, ".csv")

  raw_data <- read_csv(urlfile)

  # Could also add data on ICU
  # https://github.com/J535D165/CoronaWatchNL/blob/master/data/nice_ic_by_day.csv

  longData <- raw_data %>%
    select(date = Datum, data_type = Type, value = Aantal) %>%
    mutate(
      data_type = recode(data_type,
        "Totaal" = "confirmed",
        "Ziekenhuisopname" = "hospitalized",
        "Overleden" = "deaths"),
      country = "Netherlands",
      variable = "incidence",
      region = country,
      source = "RIVM")

  excessData <- try(getExcessDeathNL(startAt))
  if (!"try-error"  %in% class(excessData)) {
    longData <- rbind.fill(longData, excessData)
  }

  cumulData <- getCumulData(longData)
  longData <- rbind.fill(longData, cumulData)

  return(longData)
}

##### EXCESS Death NL
# install.packages("cbsodataR")
# The netherlands has developed an R package
# to access their central statistics data
# https://www.cbs.nl/en-gb/our-services/open-data/statline-as-open-data/quick-start-guide
#library("cbsodataR")

getDeathNL <- function() {
  # Death data has the number 70895NED
  # https://opendata.cbs.nl/statline/#/CBS/nl/dataset/70895ned/table?fromstatweb
  # unit: X0 is first week of year, may be partial; W1 normal weeks; JJ is whole year summed

  raw_data <- cbs_get_data("70895NED")
  data <- raw_data %>%
    select(sex = "Geslacht", age = "LeeftijdOp31December",
      period = "Perioden", deaths = "Overledenen_1") %>%
    separate(period, c("year", "unit", "week"), sep = c(4, 6)) %>%
    mutate(sex = recode(sex,
      "1100" = "all",
      "3000" = "men",
      "4000" = "women"),
    age = recode(age,
      "10000" = "all",
      "21700" = "0-65",
      "41700" = "65-80",
      "53950" = "80+"))
  return(data)
}

getRawExcessDeathNL <- function(startAt = as.Date("2020-02-20")) {

  relevant_weeks <- sprintf("%02d", seq(isoweek(startAt), isoweek(Sys.Date()) - 2))

  raw_data <- getDeathNL()

  data <- raw_data %>%
    filter(sex == "all", age == "all", unit == "W1", week %in% relevant_weeks)

  past_data <- data %>% filter(year %in% seq(2015, 2019))

  past_mean <- past_data %>%
    group_by(week) %>%
    #summarise_at(vars(deaths), mean)
    summarise_at(vars(deaths), list(avg_deaths = mean, sd_deaths = sd))

  excess_death <- data %>%
    filter(year == 2020) %>%
    select(year, week, deaths) %>%
    mutate(
      avg_deaths = past_mean$avg_deaths,
      sd_deaths = past_mean$sd_deaths,
      excess_deaths = ceiling(deaths - avg_deaths),
      perc_excess = 100 * (excess_deaths / avg_deaths),
      date = ymd(
        parse_date_time(
          paste(year, week, "Mon", sep = "/"), "Y/W/a",
          locale = "en_GB.UTF-8"
          ), locale = "en_GB.UTF-8")) %>%
    select(-year, -week)
  # this translation to a date associates the last day of the week

  return(excess_death)
}

getExcessDeathNL <- function(startAt = as.Date("2020-02-20")) {
  excess_death <- suppressWarnings(getRawExcessDeathNL(startAt))

  longData <- excess_death %>%
    select(date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      country = "Netherlands",
      variable = "incidence",
      region = country,
      source = "CBS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  return(longData)
}

##### UK Excess deaths ####################################

getRawExcessDeathUK <- function(startAt = as.Date("2020-02-20"), path_to_data = "../data/UK") {

  relevant_weeks <- seq(isoweek(startAt), isoweek(Sys.Date()) - 2)
  #last_week <- isoweek(Sys.Date()) - 2

  #url = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales"

  raw_data <- suppressWarnings(
    readxl::read_excel(
      path = file.path(path_to_data, "Excess_death_UK.xlsx"),
      sheet = "Weekly figures 2020", col_names = F))

  rowUK <- raw_data[c(5, 6, 9, 11, 19), ] %>%
    mutate(...1 = coalesce(...1, ...2)) %>%
    select(-...2)

  columnUK <- data.frame(tmp = 2:dim(rowUK)[2])
  for (row in 1:dim(rowUK)[1]) {
    rowdata <- rowUK %>%
      select(-...1) %>%
      slice(row) %>%
      unlist(., use.names = FALSE)

    newdata <- data.frame(rowdata)
    names(newdata) <- rowUK[[row, "...1"]]
    columnUK <- cbind(columnUK, newdata)
  }

  colnames(columnUK) <- c("tmp", "week", "date", "deaths", "avg_deaths", "covid_deaths")

  excess_deaths <- columnUK %>%
    select(-tmp) %>%
    filter(!is.na(deaths), week %in% relevant_weeks) %>%
    mutate(date = as.Date(date, origin = "1899-12-30", locale = "en_GB.UTF-8")) %>%
    mutate(excess_deaths = deaths - avg_deaths)

  return(excess_deaths)
}

getExcessDeathUK <- function(startAt = as.Date("2020-02-20"), path_to_data = "../data/UK") {
  excess_death <- getRawExcessDeathUK(startAt, path_to_data)

  longData <- excess_death %>%
    select(date, deaths = covid_deaths, excess_deaths) %>%
    pivot_longer(cols = c(deaths, excess_deaths), names_to = "data_type") %>%
    mutate(
      country = "United Kingdom", variable = "incidence",
      region = country,
      source = "ONS",
      value = ifelse(value < 0, 0, value))

  cumulData <- getCumulData(longData)
  longData <- rbind.fill(longData, cumulData)

  return(longData)
}


##### ECDC Confirmed Case Data #########################

getLongECDCData <- function(countries = NULL) {
  urlfile <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  world_data <- read_csv(urlfile)
  longData <- world_data %>%
    select(
      c(date = "dateRep", country = "countriesAndTerritories",
        confirmed = "cases", deaths = "deaths")) %>%
    mutate(date = dmy(date)) %>%
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>%
    mutate(
      variable = "incidence",
      country = gsub("_", " ", country),
      region = country,
      source = "ECDC")

  cumulData <- longData %>%
    group_by(country, data_type) %>%
    arrange(date) %>%
    mutate(
      value = cumsum(value),
      variable = "cumul") %>%
    arrange(country)

  longData <- rbind.fill(longData, cumulData)

  if (!is.null(countries)) {
    longData <- getCountryData(countries, data = longData)
  }

  longData[longData$value < 0, "value"] <- 0
  return(longData)
}

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

dataCHHospitalPath <- here("../ch-hospital-data/data")

dataDir <- here("app/data/temp")

##### Pull data

##### Swiss data
### just an example here with keeping all the data from different sources/countries in one dataframe and saving into one file
# rawData <- rbind(getAllSwissData(pathToHospData = dataCHHospitalPath), getLongECDCData())
CHrawData <- rbind(getAllSwissData(pathToHospData = dataCHHospitalPath, regions = cantonList))

# format data
CHrawData <- tibble::as_tibble(CHrawData) %>%
  mutate(
    region = recode(as.character(region), "CH" = "Switzerland"),
    country = recode(country, "CH" = "Switzerland"))
# save data
# pathToCHRawDataSave <- file.path(dataDir, "CH_Raw_data.Rdata")
# save(CHrawData, file = pathToCHRawDataSave)

##### European data

ECDCdata <- getLongECDCData(setdiff(countryList, c('Switzerland', 'Netherlands')))
swissExcessDeath <- getExcessDeathCH(startAt = as.Date("2020-02-20"))
NLdata <- getDataNL(stopAfter = Sys.Date() - 1)
UKExcessDeath <- getExcessDeathUK(startAt = as.Date("2020-02-20"), 
                           path_to_data = here("../ch-hospital-data/data/UK")) %>%
  filter(data_type %in% c("excess_deaths"))

EUrawData <- rbind(ECDCdata, swissExcessDeath, NLdata, UKExcessDeath)

# format data
EUrawData <- tibble::as_tibble(EUrawData)
# save data
# pathToEURawDataSave <- file.path(dataDir, "EU_Raw_data.Rdata")
# save(EUrawData, file = pathToEURawDataSave)

##### Finished pulling data

pathToRawDataSave <- file.path(dataDir, "Raw_data.Rdata")
rawData <- bind_rows(CHrawData, EUrawData) %>%
  mutate(
    data_type = factor(
      data_type,
      levels = c("confirmed", "hospitalized", "deaths", "excess_deaths"),
      labels = c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")))
save(rawData, file = pathToRawDataSave)

# figuring out when estimation can start (i.e. on the first day confirmed cases are > 100)

estimateStartDates <- rawData %>%
  filter(
    data_type == "Confirmed cases",
    !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
    country == region) %>%
  group_by(country, source, data_type, variable) %>%
  filter(
    value > 100
  ) %>%
  top_n(n = -1, value) %>%
  filter(variable == "cumul") %>%
  ungroup() %>%
  select(country, estimateStart = date)

# figuring out when estimation ends i.e. applying the delays

delays <- tibble(
  data_type = factor(
    c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths"),
    levels = c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths")),
  delay = c(10, 10, 15, 15)
)

estimateDatesDf <- rawData %>%
  filter(
    !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
    region == country
  ) %>%
  group_by(country, source, data_type) %>%
  top_n(n = 1, date) %>%
  filter(variable == "cumul") %>%
  arrange(country) %>%
  left_join(delays, by = "data_type") %>%
  ungroup() %>%
  transmute(
    country = country, data_type = data_type, estimateEnd = date - delay) %>%
  left_join(estimateStartDates, by = "country")

estimatesDates <- list()

for (i in unique(estimateDatesDf$country)) {
  tmp <- filter(estimateDatesDf, country == i)
  tmpListEnd <- tmp$estimateEnd
  names(tmpListEnd) <- tmp$data_type
  tmpListStart <- tmp$estimateStart
  names(tmpListStart) <- tmp$data_type
  estimatesDates[[i]] <- list(start = tmpListStart, end = tmpListEnd)
}
pathToEstimateDates <- file.path(dataDir, "estimate_dates.Rdata")
save(estimatesDates, file = pathToEstimateDates)

pathToCantonList <- file.path(dataDir, "cantonList.Rdata")
save(cantonList, file = pathToCantonList)

pathToCountryListSave <- file.path(dataDir, "countryList.Rdata")
save(countryList, file = pathToCountryListSave)

pathToLatestData <- file.path(dataDir, "latestData.Rdata")
latestData <- rawData %>%
  group_by(country, source) %>%
  summarize(date = max(date))
save(latestData, file = pathToLatestData)

writeLines(str_c("last check: ", Sys.time()), file.path(dataDir, "lastCheck.txt"))

print(paste("Done 1_getRawData.R:", Sys.time()))
