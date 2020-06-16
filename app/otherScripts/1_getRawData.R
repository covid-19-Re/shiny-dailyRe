startTime <- Sys.time()
save(startTime, file = "ScriptStartTime.Rdata")
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

###############################################
################ Utilities ####################
###############################################

## Get incidence data from cumulative counts data and restructure dataframe to long format
## Any data after "stoppingDate" is excluded
meltCumulativeData <- function(
  rawData,
  dataType,
  dataSource,
  country = "CH",
  nameDateCol = "Date",
  stoppingDate = (Sys.Date() - 1)) {

  cumulData <- rawData
  cumulData$Date <- ymd(cumulData$Date, locale = "en_GB.UTF-8")
  cumulData <- cumulData[cumulData$Date <= stoppingDate, ]
  cumulData <- melt(cumulData, id.vars = nameDateCol)
  colnames(cumulData) <- c("date", "region", "value")

  cumulData <- bind_rows(lapply(
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

  incidenceData <- bind_rows(lapply(
    unique(cumulData$region), function(reg) {
      incidenceSeries <- subset(cumulData, region == reg);
      incidence <- diff(incidenceSeries$value);
      incidenceSeries <- incidenceSeries[-1, ]
      incidenceSeries$value <- incidence
      return(incidenceSeries);
    }
  ))
  incidenceData$variable <- rep("incidence", nrow(incidenceData))

  return(bind_rows(cumulData, incidenceData))
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
    ## Replace missing values by the previous days value
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

calcIncidenceData <- function(data) {
  incidence <- diff(data$value)
  data <- data[-1, ]
  data$value <- incidence
  data$variable <- "incidence"
  return(data)
}

########################################
############ Data fetching #############
########################################

##### Swiss Data #######
sumGreaterRegions <- function(chData){
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

  greaterRegionsData <- chData %>%
    filter(country == "CH", region != "FL") %>%
    left_join(greaterRegions, by = "region") %>%
    ungroup() %>%
    mutate(region = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, region, source, data_type, variable) %>%
    dplyr::summarize(
      value = sum(value),
      country = "CH")
  return(greaterRegionsData)
}

getSwissDataFromOpenZH <- function(stopAfter = (Sys.Date() - 1)) {
  openZHurl <- "https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv"

  openZHraw <- read_csv(
    file = openZHurl,
    col_types = cols_only(
      date = col_date(format = ""),
      abbreviation_canton_and_fl = col_character(),
      ncumul_conf = col_double(),
      ncumul_deceased = col_double())
  ) %>%
  dplyr::rename(
    region = "abbreviation_canton_and_fl",
    confirmed = "ncumul_conf",
    deaths = "ncumul_deceased") %>%
  pivot_longer(cols = confirmed:deaths, names_to = "data_type", values_to = "value") %>%
  mutate(variable = "cumul", source = "openZH") %>%
  dplyr::select(date, region, source, data_type, value, variable) %>%
  complete(date, region, source, data_type, variable) %>%
  mutate(country = if_else(region != "FL", "CH", "FL")) %>%
  filter(date <= stopAfter)

  openZHcantons <- openZHraw  %>%
    group_by(data_type, region) %>%
    nest() %>%
    mutate(
      dataCurated = map(data, curateLongTimeSeries, isIncidenceData = FALSE),
    ) %>%
    dplyr::select(-data) %>%
    unnest(cols = "dataCurated")

  openZHsum <- openZHraw %>%
    arrange(region, data_type, variable, date) %>%
    group_by(region, data_type) %>%
    fill(value, .direction = "down") %>%
    filter(!is.na(value))

  openZHswitzerland <- openZHsum %>%
    filter(country == "CH") %>%
    group_by(date, country, source, data_type, variable) %>%
    dplyr::summarize(
      value = sum(value),
      region = "CH")

  openZHgreaterRegions <- sumGreaterRegions(openZHsum)

  openZHincidence <- bind_rows(openZHcantons, openZHswitzerland, openZHgreaterRegions) %>%
    group_by(data_type, region) %>%
    nest() %>%
    mutate(
      dataIncidence = map(data, calcIncidenceData),
    )

  openZHdata <- bind_rows(
    openZHincidence %>% dplyr::select(-dataIncidence) %>% unnest(cols = data),
    openZHincidence %>% dplyr::select(-data) %>% unnest(cols = dataIncidence)
  ) %>%
  dplyr::select(date, region, country, source, data_type, value, variable) %>%
  arrange(country, region, data_type, variable, date)

  return(openZHdata)
}

getSwissDataFromBAG <- function(path, filename = "incidence_data_CH.csv"){
  filePath <- file.path(path, filename)
  bagData <- read_csv(filePath,
    col_types = cols(
      date = col_date(format = ""),
      region = col_character(),
      country = col_character(),
      source = col_character(),
      data_type = col_character(),
      value = col_double(),
      variable = col_character()))

  bagDataGreaterRegions <- sumGreaterRegions(filter(bagData, region != "CH"))

  bagDataAll <- bind_rows(bagData, bagDataGreaterRegions)

  return(bagDataAll)
}

## Include hospitalization counts from local csv files
getHospitalData <- function(path, region = "CH", csvBaseName="Hospital_cases", dataTypeSuffix="") {
  filePath <- file.path(path, str_c(csvBaseName,dataTypeSuffix, "_", region, ".csv"))

  if (file.exists(filePath)) {
    cumData <- read_csv(filePath,
    col_types = cols(
      Date = col_date(format = ""),
      Incidence = col_double(),
      CH = col_double()))
    cumData <- cumData[, c(1, 3)]
    out <- as_tibble(meltCumulativeData(cumData,
      dataType = str_c("hospitalized", dataTypeSuffix),
      country = "CH",
      dataSource = "FOPH")) %>%
      mutate(region = as.character(region))
  } else {
    cat("Swiss Hospital Data file not found. Ignoring... \n")
    out <- NULL
  }
  return(out)
}

## Combine swiss data with hospitalization data
getAllSwissData <- function(stoppingAfter = (Sys.Date() - 1), pathToHospData) {
  #openZHData <- getSwissDataFromOpenZH(stopAfter = stoppingAfter)
  bagData <- getSwissDataFromBAG(path = pathToHospData)

  hospitalData <- getHospitalData(path = pathToHospData, region = "CH", dataTypeSuffix = "")
  hospitalData_onsets <- getHospitalData(path = pathToHospData, region = "CH", dataTypeSuffix = "_onsets")
  hospitalData_admissions <- getHospitalData(path = pathToHospData, region = "CH", dataTypeSuffix = "_admissions")
  swissData <- bind_rows(bagData, hospitalData, hospitalData_onsets, hospitalData_admissions) %>% ungroup()
  return(swissData)
}

getExcessDeathCH <- function(startAt = as.Date("2020-02-20")) {
  # urlPastData = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12607335/master"
  # pastData <- read_delim(urlPastData, delim = ";", comment = "#")

  url2020 <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/13047388/master"
  data2020 <- try(read_delim(url2020, delim = ";"))
  
  if ("try-error" %in% class(data2020)){
    return(NULL)
  }

  relevant_weeks <- seq(isoweek(startAt), isoweek(Sys.Date()) - 2)

  tidy_data <- data2020 %>%
    dplyr::select(date = Ending, week = Week, age = Age,
           avg_deaths = Expected, deaths = NoDec_EP) %>%
    filter(week %in% relevant_weeks) %>%
    mutate(date = dmy(date))

  longData <- tidy_data %>%
    group_by(date) %>%
    summarise_at(vars(deaths, avg_deaths), list(total = sum)) %>%
    mutate(excess_deaths = round(deaths_total - avg_deaths_total)) %>%
    dplyr::select(date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(country = "Switzerland", variable = "incidence",
           region = country, source = "BFS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)

  return(longData)
}

##### Excess Death HMD ##########################################

getExcessDeathHMD <- function(startAt = as.Date("2020-02-20")) {
  # Human mortality database
  
  url <- "https://www.mortality.org/Public/STMF/Outputs/stmf.csv"
  rawData <- try(read_csv(url, comment = "#"))

  if ("try-error" %in% class(rawData)){
    return(NULL)
  }
  
  tidy_data <- rawData %>%
    dplyr::select(country = CountryCode, year = Year,
           week = Week, sex = Sex, deaths = DTotal) %>%
    filter(sex == 'b') %>%
    mutate(country = recode(country, 
                AUT="Austria", BEL="Belgium",
                BGR="Bulgaria", CZE="Czech Republic", 
                DEUTNP="Germany", DNK="Denmark", ESP="Spain",     
                FIN="Finland", GBRTENW="United Kingdom", ISL="Island",
                NLD="Netherlands", NOR="Norway", PRT="Portugal", SWE="Sweden",
                USA="USA"))
  
  past_data <- tidy_data %>%
    filter(year %in% seq(2015, 2019)) %>%
    group_by(country, week) %>%
    summarise_at(vars(deaths), list(avg_deaths = mean))

  excess_death <- tidy_data %>%
    filter(year == 2020,
           week > isoweek(startAt)) %>%
    dplyr::select(country, year, week, deaths) %>%
    left_join(past_data, by = c("country", "week")) %>%
    mutate(
      excess_deaths = ceiling(deaths - avg_deaths),
      date = ymd(
        parse_date_time(
          paste(year, week, "Mon", sep = "/"), "Y/W/a",
          locale = "en_GB.UTF-8"
        ), locale = "en_GB.UTF-8")) %>%
    dplyr::select(-year, -week)

  
  longData <- excess_death %>%
    dplyr::select(date, country, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(variable = "incidence",
           region = country, source = "HMD") %>%
    mutate(value = ifelse(value < 0, 0, value))
  
  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)
  
  return(longData)
}

getExcessDeathIT <- function(filePath = here::here("../covid19-additionalData/excessDeath/Excess_death_IT.csv"), 
                             startAt = as.Date("2020-02-20")) {
  #https://www.istat.it/it/archivio/240401 
  if (file.exists(filePath)) {
    rawData <- read_csv(filePath,
                        col_types = cols(
                          REG = col_double(),
                          PROV = col_double(),
                          NOME_REGIONE = col_character(),
                          NOME_PROVINCIA = col_character(),
                          NOME_COMUNE = col_character(),
                          COD_PROVCOM = col_character(),
                          TIPO_COMUNE = col_double(),
                          CL_ETA = col_double(),
                          GE = col_character(),
                          M_15 = col_double(),
                          F_15 = col_double(),
                          T_15 = col_double(),
                          M_16 = col_double(),
                          F_16 = col_double(),
                          T_16 = col_double(),
                          M_17 = col_double(),
                          F_17 = col_double(),
                          T_17 = col_double(),
                          M_18 = col_double(),
                          F_18 = col_double(),
                          T_18 = col_double(),
                          M_19 = col_double(),
                          F_19 = col_double(),
                          T_19 = col_double(),
                          M_20 = col_character(),
                          F_20 = col_character(),
                          T_20 = col_character()
                        ))
  } else {
    return(NULL)
  }

  tidy_data <- rawData %>%
    dplyr::select(commune_code = COD_PROVCOM, date = GE,
                  paste0("T_", seq(15, 20))) %>%
    mutate(T_20 = parse_double(T_20, na = "n.d.")) %>%
    pivot_longer(cols = paste0("T_", seq(15, 20)), names_to = 'year', values_to = 'deaths') %>%
    mutate(year = gsub('T_','20', year),
           day = date,
           date = paste0(year, date)) %>%
    filter(day != "0229") %>%
    mutate(date = parse_date(date, format = "%Y%m%d"),
           commune_day = paste0(commune_code, "_", day))
  # we exclude the day 02-29 because it does not exist in all years
  
  # in 2020 for some communes data is missing on some days
  # we exclude it, so we don't compare to a wrong total
  na_commune_days <- tidy_data %>%
    filter(year == "2020",
           is.na(deaths)) %>%
    pull(unique(commune_day))
  
  past_data <- tidy_data %>%
    filter(!(commune_day %in% na_commune_days),
      year %in% seq(2015, 2019)) %>%
    group_by(date, year, day) %>%
    summarise_at(vars(deaths), sum, na.rm = TRUE) %>%
    group_by(day) %>%
    summarise_at(vars(deaths), list(avg_deaths = mean))
  
  excess_death <- tidy_data %>%
    group_by(date, year, day) %>%
    summarise_at(vars(deaths), sum, na.rm = TRUE) %>%
    filter(year == "2020",
           day > "0220") %>%
    left_join(past_data, by = c("day")) %>%
    mutate(excess_deaths = ceiling(deaths - avg_deaths)) %>%
    dplyr::select(-day) %>% 
    ungroup()
  
  longData <- excess_death %>%
    dplyr::select(date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(variable = "incidence", country = "Italy",
           region = country, source = "Istat") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(!is.na(value))
  
  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)
  
  return(longData)
}

##### French Data ##########################################

getHospitalDataFR <- function(){
  # nouveaux file contains the incidence
  url = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
  rawData = try(read_delim(url, delim = ";",
                       col_types = cols(
                         dep = col_character(),
                         jour = col_date(format = ""),
                         incid_hosp = col_double(),
                         incid_rea = col_double(),
                         incid_dc = col_double(),
                         incid_rad = col_double()
                       )))
  if('try-error' %in% class(rawData)){
    return(NULL)
  }

  longData <- rawData %>%
    select(date = jour,
           region = dep,
           incid_hosp) %>%
    group_by(date) %>%
    summarise_at(vars(incid_hosp), list(value = sum) ) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(data_type = 'hospitalized',
           country = "France",
           variable = "incidence",
           region = country,
           source = "SpF-DMI")
  
  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)

  return(longData)
}

getExcessDeathFR <- function(startAt = as.Date("2020-02-20")){
  url <- "https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/france_excess_deaths.csv"
  rawData = try(read_csv(url,
                         col_types = cols(
                           country = col_character(),
                           region = col_character(),
                           region_code = col_double(),
                           start_date = col_date(format = ""),
                           end_date = col_date(format = ""),
                           year = col_double(),
                           week = col_double(),
                           population = col_double(),
                           total_deaths = col_double(),
                           covid_deaths = col_double(),
                           expected_deaths = col_double(),
                           excess_deaths = col_double(),
                           non_covid_deaths = col_double()
                         )))
  
  if('try-error' %in% class(rawData)){
    return(NULL)
  }
  
  longData <- rawData %>%
    group_by(country, end_date, week) %>%
    summarise_at(vars(total_deaths, excess_deaths), list(sum)) %>%
    dplyr::select(country, date = end_date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(variable = "incidence",
           region = country,
          source = "Economist") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(date > startAt)
  
  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)
  
  return(longData)
}

##### Belgian Data ##########################################

getHospitalDataBE <- function(){
  url = "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  rawData = try(read_csv(url,
                           col_types = cols(
                             DATE = col_date(format = ""),
                             PROVINCE = col_character(),
                             REGION = col_character(),
                             NR_REPORTING = col_double(),
                             TOTAL_IN = col_double(),
                             TOTAL_IN_ICU = col_double(),
                             TOTAL_IN_RESP = col_double(),
                             TOTAL_IN_ECMO = col_double(),
                             NEW_IN = col_double(),
                             NEW_OUT = col_double()
                           ) ))
  if('try-error' %in% class(rawData)){
    return(NULL)
  }
  
  longData <- rawData %>%
    select(date = DATE,
           value = NEW_IN) %>%
    group_by(date) %>%
    summarise_at(vars(value), list(value = sum) ) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(data_type = 'hospitalized',
           country = "Belgium",
           variable = "incidence",
           region = country,
           source = "Sciensano")
  
  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)
  
  return(longData)
}


##### Dutch Data ##########################################
getDataNL <- function(stopAfter = Sys.Date(), startAt = as.Date("2020-02-20")) {
  baseurl <- str_c("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/",
    "rivm_NL_covid19_national_by_date/rivm_NL_covid19_national_by_date_")

  urlfile <- paste0(baseurl, stopAfter, ".csv")
  raw_data <- try(read_csv(urlfile))
  
  if ('try-error' %in% class(raw_data)){
    urlfile <- paste0(baseurl, "latest.csv")
    raw_data <- read_csv(urlfile)
  }

  # Could also add data on ICU
  # https://github.com/J535D165/CoronaWatchNL/blob/master/data/nice_ic_by_day.csv

  longData <- raw_data %>%
    dplyr::select(date = Datum, data_type = Type, value = Aantal) %>%
    mutate(
      data_type = recode(data_type,
        "Totaal" = "confirmed",
        "Ziekenhuisopname" = "hospitalized",
        "Overleden" = "deaths"),
      country = "Netherlands",
      variable = "incidence",
      region = country,
      source = "RIVM")

  # excessData <- try(getExcessDeathNL(startAt))
  # if (!"try-error"  %in% class(excessData)) {
  #   longData <- bind_rows(longData, excessData)
  # }

  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)

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
    dplyr::select(sex = "Geslacht", age = "LeeftijdOp31December",
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
    dplyr::select(year, week, deaths) %>%
    left_join(past_mean, by = "week") %>%
    mutate(
      excess_deaths = ceiling(deaths - avg_deaths),
      perc_excess = 100 * (excess_deaths / avg_deaths),
      date = ymd(
        parse_date_time(
          paste(year, week, "Mon", sep = "/"), "Y/W/a",
          locale = "en_GB.UTF-8"
          ), locale = "en_GB.UTF-8")) %>%
    dplyr::select(-year, -week)
  # this translation to a date associates the last day of the week

  return(excess_death)
}

getExcessDeathNL <- function(startAt = as.Date("2020-02-20")) {
  excess_death <- suppressWarnings(getRawExcessDeathNL(startAt))

  longData <- excess_death %>%
    dplyr::select(date, excess_deaths) %>%
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

  # url <- str_c("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/",
  #   "deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales")

  raw_data <- suppressWarnings(
    readxl::read_excel(
      path = path_to_data,
      sheet = "Weekly figures 2020", col_names = F))

  rowUK <- raw_data[c(5, 6, 9, 11, 19), ] %>%
    mutate(...1 = coalesce(...1, ...2)) %>%
   dplyr::select(-...2)

  columnUK <- data.frame(tmp = 2:dim(rowUK)[2])
  for (row in 1:dim(rowUK)[1]) {
    rowdata <- rowUK %>%
      dplyr::select(-...1) %>%
      slice(row) %>%
      unlist(., use.names = FALSE)

    newdata <- data.frame(rowdata)
    names(newdata) <- rowUK[[row, "...1"]]
    columnUK <- cbind(columnUK, newdata)
  }

  colnames(columnUK) <- c("tmp", "week", "date", "deaths", "avg_deaths", "covid_deaths")

  excess_deaths <- columnUK %>%
    dplyr::select(-tmp) %>%
    filter(!is.na(deaths), week %in% relevant_weeks) %>%
    mutate(date = as.Date(date, origin = "1899-12-30", locale = "en_GB.UTF-8")) %>%
    mutate(excess_deaths = deaths - avg_deaths)

  return(excess_deaths)
}

getExcessDeathUK <- function(startAt = as.Date("2020-02-20"), path_to_data = "../data/UK") {
  excess_death <- getRawExcessDeathUK(startAt, path_to_data)

  longData <- excess_death %>%
    dplyr::select(date, deaths = covid_deaths, excess_deaths) %>%
    pivot_longer(cols = c(deaths, excess_deaths), names_to = "data_type") %>%
    mutate(
      country = "United Kingdom", variable = "incidence",
      region = country,
      source = "ONS",
      value = ifelse(value < 0, 0, value))

  cumulData <- getCumulData(longData)
  longData <- bind_rows(longData, cumulData)

  return(longData)
}


##### ECDC Confirmed Case Data #########################

getLongECDCData <- function(countries = NULL) {
  urlfile <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  world_data <- read_csv(urlfile)
  longData <- world_data %>%
    dplyr::select(
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

  longData <- bind_rows(longData, cumulData)

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

dataCHHospitalPath <- here::here("../ch-hospital-data/data/CH")

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
cat(paste("CH"))

##### European data
countryList <- c("Austria", "Belgium", "France", "Germany", "Italy",
  "Netherlands", "Spain", "Switzerland", "Sweden", "United Kingdom")

ECDCdata <- getLongECDCData(setdiff(countryList, c("Switzerland", "Netherlands")))
swissExcessDeath <- getExcessDeathCH(startAt = as.Date("2020-02-20"))
cat(paste("Swiss Excess"))

NLdata <- try(getDataNL(stopAfter = Sys.Date() - 1))
if ('try-error' %in% class(NLdata)){
  NLdata <- NULL
}
cat(paste("NL"))

ExcessDeathData <- getExcessDeathHMD() %>%
  filter(country %in% countryList)
cat(paste("HMD"))

pathToExcessDeathIT <- here::here("../covid19-additionalData/excessDeath/Excess_death_IT.csv")
ITExcessDeath <- getExcessDeathIT(filePath = pathToExcessDeathIT, 
                 startAt = as.Date("2020-02-20"))
cat(paste("IT"))

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

EUrawData <- bind_rows(ECDCdata, swissExcessDeath, NLdata, ExcessDeathData,
                       ITExcessDeath, hospitalDataFR, ExcessDeathFR,
                       hospitalDataBE) %>%
  as_tibble()
print("Bound")
# save data
# pathToEURawDataSave <- file.path(dataDir, "EU_Raw_data.Rdata")
# save(EUrawData, file = pathToEURawDataSave)

##### Finished pulling data

pathToRawDataSave <- file.path(dataDir, "Raw_data.Rdata")

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

save(rawData, file = pathToRawDataSave)

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
  delay = c(10, 10, 15, 30)
)

estimateDatesDf <- rawData %>%
  filter(
    !(country == "Switzerland" & region == "Switzerland" & source == "ECDC"),
    variable == "cumul"
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

pathToEstimateDates <- file.path(dataDir, "estimate_dates.Rdata")
save(estimatesDates, file = pathToEstimateDates)

validEstimates <- estimateDatesDf %>%
  filter(!is.na(estimateStart)) %>%
  dplyr::select(country, region) %>%
  distinct()

pathToValidEstimates <- file.path(dataDir, "valid_estimates.Rdata")
save(validEstimates, file = pathToValidEstimates)

pathToCountryListSave <- file.path(dataDir, "countryList.Rdata")
countryList <- unique(validEstimates$country)
save(countryList, file = pathToCountryListSave)

pathToLatestData <- file.path(dataDir, "latestData.Rdata")
  
latestData <- rawData %>%
  mutate(data_type=replace(data_type, data_type %in% c("Hospitalized patients - onset", "Hospitalized patients - admission"), "Hospitalized patients")) %>%
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
      "SpF-DMI","Data from the French National Public Health Agency", "https://www.data.gouv.fr/fr/datasets/donnees-hospitalieres-relatives-a-lepidemie-de-covid-19/",
      "Economist","Data from the Economist Excess Death Tracker", "https://github.com/TheEconomist/covid-19-excess-deaths-tracker",
      "Sciensano","Data from the Belgian Institute for Health", "https://epistat.wiv-isp.be/covid/"
    ), by = "source")

save(latestData, file = pathToLatestData)

cat(paste("###", Sys.time(), "- done 1_getRawData.R", "\n"))
