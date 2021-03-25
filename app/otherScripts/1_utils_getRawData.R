###############################################
################ Utilities ####################
###############################################

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

calcIncidenceData <- function(data) {
  incidence <- diff(data$value)
  data <- data[-1, ]
  data$value <- incidence
  return(data)
}

########################################
############ Data fetching #############
########################################

##### ECDC #####

getDataECDC <- function(countries = NULL, tempFileName = NULL, tReload = 15) {
  urlfile <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  if (is.null(tempFileName)) {
    csvPath <- urlfile
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(urlfile, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new ECDC data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new ECDC data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  world_data <- try(read_csv(csvPath,
                             col_types = cols_only(
                               dateRep = col_date(format = "%d/%m/%Y"),
                               countryterritoryCode = col_character(),
                               cases = col_double(),
                               deaths = col_double()
                             )
  ))

  if ("try-error" %in% class(world_data)) {
    warning(str_c("couldn't get ECDC data from ", url, "."))
    return(NULL)
  }

  longData <- world_data %>%
    dplyr::select(
      date = "dateRep",
      countryIso3 = "countryterritoryCode",
      region = "countryterritoryCode",
      confirmed = "cases", deaths = "deaths") %>%
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>%
    mutate(
      date_type = "report",
      local_infection = TRUE,
      source = "ECDC") %>%
    filter(!is.na(value))

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }

  longData[longData$value < 0, "value"] <- 0
  return(longData)
}


##### Our World in Data #####
getDataOWID <- function(countries = NULL, tempFileName = NULL, tReload = 300) {
  urlfile <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

  if (is.null(tempFileName)) {
    csvPath <- urlfile
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(urlfile, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new OWID data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new OWID data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  world_data <- try(read_csv(csvPath,
    col_types = cols_only(
      iso_code = col_character(),
      date = col_date(format = ""),
      new_cases = col_double(),
      new_deaths = col_double())
    ))


  if ("try-error" %in% class(world_data)) {
    warning(str_c("couldn't get OWID data from ", url, "."))
    return(NULL)
  }

  longData <- world_data %>%
    dplyr::select(
      date = "date",
      countryIso3 = "iso_code",
      region = "iso_code",
      confirmed = "new_cases", deaths = "new_deaths") %>%
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>%
    mutate(
      date_type = "report",
      local_infection = TRUE,
      source = "OWID") %>%
    filter(!is.na(value))

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }

  longData[longData$value < 0, "value"] <- 0
  return(longData)
}

getVaccinationDataOWID <- function(countries = NULL, tempFileName = NULL, tReload = 300) {
  urlfile <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"

  if (is.null(tempFileName)) {
    csvPath <- urlfile
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(urlfile, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new OWID Vaccination data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new OWID Vaccination data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  world_data <- try(read_csv(csvPath,
    col_types = cols_only(
      iso_code = col_character(),
      date = col_date(format = ""),
      total_vaccinations = col_double(),
      people_vaccinated = col_double(),
      people_fully_vaccinated = col_double(),
      daily_vaccinations_raw = col_double(),
      daily_vaccinations = col_double(),
      total_vaccinations_per_hundred = col_double(),
      people_vaccinated_per_hundred = col_double(),
      people_fully_vaccinated_per_hundred = col_double(),
      daily_vaccinations_per_million = col_double()
    )
  ))

  if ("try-error" %in% class(world_data)) {
    warning(str_c("couldn't get OWID Vaccination data from ", url, "."))
    return(NULL)
  }

  longData <- world_data %>%
    rename(countryIso3 = iso_code) %>%
    pivot_longer(cols = total_vaccinations:daily_vaccinations_per_million, names_to = "data_type") %>%
    mutate(
      region = countryIso3,
      source = "OWID") %>%
    filter(!is.na(value))

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }
  return(longData)
}

##### Human Mortality Database #####

getExcessDeathHMD <- function(countries = NULL, startAt = as.Date("2020-02-20"), tempFileName = NULL, tReload = 15) {
  urlfile <- "https://www.mortality.org/Public/STMF/Outputs/stmf.csv"
  if (is.null(tempFileName)) {
    csvPath <- urlfile
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(urlfile, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new HMD data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new HMD data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  rawData <- try(
    read_csv(csvPath, comment = "#",
             col_types = cols(
               .default = col_double(),
               CountryCode = col_character(),
               Sex = col_character()
             )
    )
  )

  if ("try-error" %in% class(rawData)) {
    warning(str_c("couldn't get mortality data from ", csvPath, "."))
    return(NULL)
  }

  tidy_data <- rawData %>%
    dplyr::select(countryIso3 = CountryCode, year = Year,
                  week = Week, sex = Sex, deaths = DTotal) %>%
    filter(sex == "b") %>%
    mutate(
      countryIso3 = recode(
        countryIso3,
        DEUTNP = "DEU", GBRTENW = "GBR"),
      region = countryIso3)

  past_data <- tidy_data %>%
    filter(year %in% seq(2015, 2019)) %>%
    group_by(countryIso3, week) %>%
    summarise(avg_deaths = mean(deaths), .groups = "keep")

  excess_death <- tidy_data %>%
    filter(year == 2020,
           week > isoweek(startAt)) %>%
    dplyr::select(countryIso3, region, year, week, deaths) %>%
    left_join(past_data, by = c("countryIso3", "week")) %>%
    mutate(
      excess_deaths = ceiling(deaths - avg_deaths),
      date = ymd(
        parse_date_time(
          paste(year, week, "Mon", sep = "/"), "Y/W/a",
          locale = "en_GB.UTF-8"
        ), locale = "en_GB.UTF-8")) %>%
    dplyr::select(-year, -week)

  longData <- excess_death %>%
    dplyr::select(date, countryIso3, region, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      local_infection = TRUE,
      date_type = "report",
      source = "HMD") %>%
    mutate(value = if_else(value < 0, 0, value))

  if (!is.null(countries)) {
    longData <- filter(
      longData,
      countryIso3 %in% countries
    )
  }

  return(longData)
}


##### Belgium #####

getHospitalDataBEL <- function() {
  url <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  rawData <- try(
    read_csv(url,
             col_types = cols(
               .default = col_double(),
               DATE = col_date(format = ""),
               PROVINCE = col_character(),
               REGION = col_character()
             )
    )
  )

  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read belgian hospital data at ", url))
    return(NULL)
  }

  longData <- rawData %>%
    dplyr::select(date = DATE,
                  value = NEW_IN) %>%
    group_by(date) %>%
    summarise_at(vars(value), list(value = sum)) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(
      data_type = "hospitalized",
      countryIso3 = "BEL",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "Sciensano")
  
  return(longData)
}

getCaseDataBEL <- function() {
  url <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  rawData <- try(
    read_csv(url,
             col_types = cols(
               .default = col_double(),
               DATE = col_date(format = ""),
               PROVINCE = col_character(),
               REGION = col_character(),
               SEX = col_character(),
               AGEGROUP = col_character()
             )
    )
  )
  
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read belgian case data at ", url))
    return(NULL)
  }
  
  longData <- rawData %>%
    dplyr::select(date = DATE,
                  value = CASES) %>%
    group_by(date) %>%
    summarise_at(vars(value), list(value = sum)) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(
      data_type = "confirmed",
      countryIso3 = "BEL",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "Sciensano") %>%
    filter(
      !is.na(date)
    )
  
  return(longData)
}

getDeathDataBEL <- function() {
  url <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
  rawData <- try(
    read_csv(url, col_types = cols(
      .default = col_character(),
      DATE = col_date(format = ""),
      DEATHS = col_double()
    ))
  )
  
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read belgian case data at ", url))
    return(NULL)
  }
  
  longData <- rawData %>%
    dplyr::select(date = DATE,
                  value = DEATHS) %>%
    group_by(date) %>%
    summarise_at(vars(value), list(value = sum)) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(
      data_type = "deaths",
      countryIso3 = "BEL",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "Sciensano")
  
  return(longData)
}

getDataBEL <- function() {
  caseData <- getCaseDataBEL()
  deathData <- getDeathDataBEL()
  excessDeath <- NULL#getExcessDeathHMD(countries = "BEL", tempFileName = HMDtemp, tReload = tReload)
  hospitalData <- getHospitalDataBEL()
  allData <- bind_rows(caseData, deathData, excessDeath, hospitalData)
  return(allData)
}





##### Chile #####

#TODO finish
# getCaseAndDeathDataCHL <- function(){
#   url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv"
#   raw_data <- try(read_csv(url))
#   if ("try-error" %in% class(raw_data)) {
#     warning(str_c("Couldn't read CHL case data at ", url))
#     return(NULL)
#   }
#   
#   long_data <- raw_data %>% 
#     filter(Fecha == "Casos nuevos totales" | Fecha == "Fallecidos") %>% 
#     pivot_longer(-c(Fecha)) %>% 
#     rename(data_type = Fecha,
#            date = name) %>% 
#     mutate(date = as.Date(date),
#            data_type = recode(data_type, "Fallecidos" = "deaths", "Casos nuevos totales"= "confirmed")) %>% 
# 
#     death_data <- long_data %>%
#       filter(data_type == "deaths") %>% 
#       mutate(value = diff(value))
# }



##### Czech Republic #####

getDataCZE <- function(){
  case_data <- getCaseDataCZE()
  death_data <- getDeathDataCZE()
  
  return(rbind(case_data, death_data))
}


getCaseDataCZE <- function(){
  
  
  url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.csv"
  raw_data <- try(read_csv(url))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read CZE case data at ", url))
    return(NULL)
  }
  
  case_data <- raw_data %>% 
    dplyr::select(datum) %>% 
    group_by(datum) %>% 
    summarize(value = n()) %>% 
    rename(date = datum) %>% 
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(value = 0)) %>% 
    mutate(countryIso3 = "CZE",
           local_infection = TRUE,
           data_type = "confirmed",
           date_type = "report",
           region = "CZE",
           source = "data.gov.cz")
  
  return(case_data)
}

getDeathDataCZE <- function(){
  url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/umrti.csv"
  raw_data <- try(read_csv(url))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read CZE death data at ", url))
    return(NULL)
  }
  
  death_data <- raw_data %>% 
    dplyr::select(datum) %>% 
    group_by(datum) %>% 
    summarize(value = n()) %>% 
    rename(date = datum) %>% 
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(value = 0)) %>% 
    mutate(countryIso3 = "CZE",
           local_infection = TRUE,
           data_type = "deaths",
           date_type = "report",
           region = "CZE",
           source = "data.gov.cz")
  
  return(death_data)
}


##### Estonia #####

getDataEST <- function(tempFile = NULL, tReload = 15) {
  case_data <- getCaseDataEST()
  hosp_data <- getHospDataEST()
  death_data <- getDataOWID(countries = "EST", tempFileName = tempFile, tReload = tReload) %>%  filter(data_type == "deaths")

  all_data <- rbind(case_data, hosp_data, death_data)
  
  return(all_data)
}

getCaseDataEST <- function() {
  url <- "https://opendata.digilugu.ee/opendata_covid19_tests_total.csv"
  raw_data <- try(read_csv(url))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read EST case data at ", url))
    return(NULL)
  }
  
  case_data <- raw_data %>% 
    transmute(date = as_date(StatisticsDate),
              value = DailyCases) %>% 
    mutate(countryIso3 = "EST",
           local_infection = TRUE,
           data_type = "confirmed",
           date_type = "report",
           region = "EST",
           source = "Terviseamet")
  return(case_data)
}

getHospDataEST <- function() {
  url <- "https://opendata.digilugu.ee/opendata_covid19_hospitalization_timeline.csv"
  raw_data <- try(read_csv(url))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read EST hosp data at ", url))
    return(NULL)
  }
  
  hosp_data <- raw_data %>% 
    transmute(date = as_date(StatisticsDate),
              value = NewCases) %>% 
    mutate(countryIso3 = "EST",
           local_infection = TRUE,
           data_type = "hospitalized",
           date_type = "report",
           region = "EST",
           source = "Terviseamet")
  return(hosp_data)
}


##### France #######

getHospitalDataFRA <- function() {
  # nouveaux file contains the incidence
  url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
  rawData <- try(read_delim(url, delim = ";",
                            col_types = cols(
                              dep = col_character(),
                              jour = col_date(format = ""),
                              incid_hosp = col_double(),
                              incid_rea = col_double(),
                              incid_dc = col_double(),
                              incid_rad = col_double()
                            )))
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read french hospital data at ", url))
    return(NULL)
  }

  longData <- rawData %>%
    dplyr::select(date = jour,
                  region = dep,
                  incid_hosp) %>%
    group_by(date) %>%
    summarise_at(vars(incid_hosp), list(value = sum)) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(data_type = "hospitalized",
           countryIso3 = "FRA",
           date_type = "report",
           local_infection = TRUE,
           region = "FRA",
           source = "SpF-DMI")

  return(longData)
}

getCaseDataFRA <- function() {
  url <- "https://www.data.gouv.fr/fr/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c"
  rawData <- try(read_delim(url, delim = ";",
                            col_types = cols(
                              fra = col_character(),
                              jour = col_date(format = ""),
                              P_f = col_double(),
                              P_h = col_double(),
                              P = col_double(),
                              pop_f = col_double(),
                              pop_h = col_double(),
                              pop = col_double(),
                              cl_age90 = col_character()
                            )))
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read french case data at ", url))
    return(NULL)
  }

  longData <- rawData %>%
    filter(cl_age90 == 0) %>%
    dplyr::select(date = jour,
                  value = P) %>%
    arrange(date) %>%
    mutate(data_type = "confirmed",
           countryIso3 = "FRA",
           date_type = "report",
           local_infection = TRUE,
           region = "FRA",
           #source = "ECDC - SpF-DMI")
           source = "OWID - SpF-DMI")

  min_date_SPF <- min(longData$date)

  #ecdcData <- getDataECDC(countries = "FRA", tempFileName = NULL, tReload = 15)

  # ecdcData <- ecdcData %>%
  #   filter(data_type == "confirmed", date <  min_date_SPF) %>%
  #   arrange(date) %>% 
  #   mutate(source = "ECDC - SpF-DMI")
  
  owidData <- getDataOWID(countries = "FRA", tempFileName = NULL, tReload = 15)
  # this is quite ugly (non standardised data)
  owidData <- owidData %>%
       filter(data_type == "confirmed", date <  min_date_SPF) %>%
       arrange(date) %>% 
       mutate(source = "OWID - SpF-DMI")
  
  #return(bind_rows(ecdcData, longData))
  return(bind_rows(owidData, longData))
}


getHospitalAndDeathDataFRA <- function() {
  # nouveaux file contains the incidence
  url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
  rawData <- try(read_delim(url, delim = ";",
                            col_types = cols(
                              dep = col_character(),
                              jour = col_date(format = ""),
                              incid_hosp = col_double(),
                              incid_rea = col_double(),
                              incid_dc = col_double(),
                              incid_rad = col_double()
                            )))
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read french hospital data at ", url))
    return(NULL)
  }
  
  longData <- rawData %>%
    dplyr::select(date = jour,
                  region = dep,
                  incid_hosp,
                  incid_dc) %>%
    group_by(date) %>%
    dplyr::summarise(hospitalized = sum(incid_hosp), deaths = sum(incid_dc), .groups = "drop") %>% 
    pivot_longer(cols = c(hospitalized, deaths),
                 names_to = "data_type") %>%
    arrange(date) %>% 
    mutate(countryIso3 = "FRA",
           date_type = "report",
           local_infection = TRUE,
           region = "FRA",
           source = "SpF-DMI")
  
  return(longData)
}

getExcessDeathFRA <- function(startAt = as.Date("2020-02-20")) {
  url <- str_c("https://raw.githubusercontent.com/TheEconomist/",
               "covid-19-excess-deaths-tracker/master/output-data/excess-deaths/france_excess_deaths.csv")
  rawData <- try(
    read_csv(url,
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
             )
    )
  )
  
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read french excess death data at ", url))
    return(NULL)
  }
  
  longData <- rawData %>%
    group_by(country, end_date, week) %>%
    summarise(
      across(c(total_deaths, excess_deaths), sum),
      .groups = "drop") %>%
    dplyr::select(date = end_date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      countryIso3 = "FRA",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "Economist") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(date > startAt)
  
  return(longData)
}

getDataFRA <- function(){
  caseData <- getCaseDataFRA()
  excessDeath <- NULL #getExcessDeathFRA()
  hospitalAndDeathData <- getHospitalAndDeathDataFRA()
  
  allData <- bind_rows(caseData, excessDeath, hospitalAndDeathData)
  
  return(allData)
}


##### Germany #####

getCaseDataDEU <- function(data_path = here::here("app/data/DEU"), filename = "incidence_data_DEU.csv") {
  filePath <- file.path(data_path, filename)
  DEUdata <- read_csv(filePath,
                      col_types = cols(
                        date = col_date(format = ""),
                        region = col_character(),
                        countryIso3 = col_character(),
                        source = col_character(),
                        data_type = col_character(),
                        value = col_double(),
                        date_type = col_character(),
                        local_infection = col_logical()))
  
  return(DEUdata)
}

getDataDEU <- function(tempFile = NULL, tReload = 15) {
  deathData <- getDataOWID(countries = "DEU", tempFileName = tempFile, tReload = tReload) %>% filter(data_type == "deaths")
  # unfortunately this code is hacky as hell, but otherwise line 42-46 in sumData fails
  deathData_dups <- deathData %>%
                    mutate(date_type = "report_plotting",
                           local_infection = NA)
  caseData <- getCaseDataDEU()
  excessDeath <- NULL
  allData <- bind_rows(caseData, deathData, deathData_dups, excessDeath)
  return(allData)
}



##### United States

## linelist data

# url <-  "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD"
# cases <- read_csv(url)
#https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD

##### Hong Kong #####

getDataHKG <- function(data_path = here::here("app/data/HKG"), filename = "incidence_data_HKG.csv") {
  filePath <- file.path(data_path, filename)
  HKdata <- read_csv(filePath,
                     col_types = cols(
                       date = col_date(format = ""),
                       region = col_character(),
                       countryIso3 = col_character(),
                       source = col_character(),
                       data_type = col_character(),
                       value = col_double(),
                       date_type = col_character(),
                       local_infection = col_logical()))
  
  return(HKdata)
}

##### Latvia #####
getDataLVA <- function( ) {
  
  url <- "https://data.gov.lv/dati/dataset/f01ada0a-2e77-4a82-8ba2-09cf0cf90db3/resource/d499d2f0-b1ea-4ba2-9600-2c701b03bd4a/download/covid_19_izmeklejumi_rezultati.csv"
  
  raw_data <- try( read_delim(url, delim=";", na=c("", "NA", "...")))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read LVA data at ", url))
    return(NULL)
  }
  
  all_data <- raw_data %>% 
    dplyr::select(Datums, ApstiprinataCOVID19InfekcijaSkaits, MirusoPersonuSkaits) %>%
    dplyr::rename(date = Datums,
                  confirmed = ApstiprinataCOVID19InfekcijaSkaits,
                  deaths = MirusoPersonuSkaits) %>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>% 
    mutate(date = as.Date(date, format="%Y.%m.%d."),
           countryIso3 = "LVA",
           local_infection = TRUE,
           date_type = "report",
           region = "LVA",
           source = "data.gov.lv")
  
  return(all_data)
}



##### Ireland #####

getDataIRL <- function(){
  hospData <- getHospDataIRL()
  caseData <- getCaseAndDeathDataIRL()
  allData <- bind_rows(caseData, hospData)
  return(allData)
}

getCaseAndDeathDataIRL <- function(){
  url <- "https://opendata-geohive.hub.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
  raw_data <- try(read_csv(url, na = c("", "NA", "None")))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read IRL data at ", url))
    return(NULL)
  }
  all_data <- raw_data %>% 
    transmute(date = as.Date(Date),
              confirmed = ConfirmedCovidCases,
              deaths = ConfirmedCovidDeaths) %>% 
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(confirmed = 0,
                         deaths = 0)) %>% 
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>% 
    mutate(region = "IRL",
           local_infection = TRUE,
           countryIso3 = "IRL",
           source = "HPSC",
           date_type = "report")
  
}

getHospDataIRL <- function(){
  url <- "https://opendata.arcgis.com/datasets/fe9bb23592ec4142a4f4c2c9bd32f749_0.csv"
  
  raw_data <- try(read_csv(url))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read IRL data at ", url))
    return(NULL)
  }
  confirmed_data <- raw_data %>% 
    transmute(date = as.Date(Date),
              value = SUM_no_new_admissions_covid19_p) %>% 
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(value = 0)) %>% 
    mutate(region = "IRL",
           data_type = "hospitalized",
           local_infection = TRUE,
           countryIso3 = "IRL",
           source = "HPSC - HSE",
           date_type = "report")
  
  return(confirmed_data)
}


##### Italy #######

getExcessDeathITA <- function(filePath = here::here("../covid19-additionalData/excessDeath/Excess_death_IT.csv"),
                              startAt = as.Date("2020-02-20")) {
  #https://www.istat.it/it/archivio/240401
  if (file.exists(filePath)) {
    rawData <- read_csv(filePath,
                        col_types = cols(
                          .default = col_double(),
                          NOME_REGIONE = col_character(),
                          NOME_PROVINCIA = col_character(),
                          NOME_COMUNE = col_character(),
                          COD_PROVCOM = col_character(),
                          GE = col_character(),
                          M_20 = col_character(),
                          F_20 = col_character(),
                          T_20 = col_character()
                        )
    )
  } else {
    return(NULL)
  }
  
  tidy_data <- rawData %>%
    dplyr::select(commune_code = COD_PROVCOM, date = GE,
                  paste0("T_", seq(15, 20))) %>%
    mutate(T_20 = parse_double(T_20, na = "n.d.")) %>%
    pivot_longer(cols = paste0("T_", seq(15, 20)), names_to = "year", values_to = "deaths") %>%
    mutate(year = gsub("T_", "20", year),
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
    mutate(
      local_infection = TRUE,
      date_type = "report",
      countryIso3 = "ITA",
      region = "ITA",
      source = "Istat") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(!is.na(value))
  
  return(longData)
}

getITADataPCM <- function(){
  url <- str_c("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/",
               "dpc-covid19-ita-andamento-nazionale.csv")
  
  rawData <- try(read_csv(
    file = url,
    col_types = cols_only(
      data = col_datetime(format = ""),
      # stato = col_character(),
      # ricoverati_con_sintomi = col_double(), # hospitalized, total
      # terapia_intensiva = col_double(), # intensive cate, total
      # totale_ospedalizzati = col_double(), # total in hopital = sum of last wtwo
      # isolamento_domiciliare = col_double(),
      # totale_positivi = col_double(), # total pos
      # variazione_totale_positivi = col_double(),
      nuovi_positivi = col_double(), # new cases
      # dimessi_guariti = col_double(), # people released from hospt
      deceduti = col_double() # deaths
      # totale_casi = col_double() # total cases
      # tamponi = col_double(), # swabs
      # casi_testati = col_double(), # cases tested
      #note_it = col_character(),
      #note_en = col_character()
    )))
  
  if ("try-error" %in% class(rawData)) {
    return(NULL)
  }
  
  data <- rawData %>%
    transmute(
      date = as.Date(data),
      countryIso3 = "ITA",
      region = "ITA",
      source = "PCM-DPC",
      date_type = "report",
      local_infection = TRUE,
      confirmed = nuovi_positivi,
      deaths = diff(c(0, deceduti))
    ) %>%
    # fix negative deaths
    mutate(deaths = if_else(deaths < 0, 0, deaths)) %>%
    pivot_longer(cols = confirmed:deaths, names_to = "data_type", values_to = "value") %>%
    arrange(countryIso3, region, source, date_type, data_type, date)
  
  return(data)
}

getDataITA <- function(tempFile = NULL, tReload = 15) {
  caseData <- getITADataPCM()
  excessDeath <- NULL#getExcessDeathITA()
  
  allData <- bind_rows(caseData, excessDeath)
  
  return(allData)
}

##### Netherlands #####

getCaseDataNLD <- function(){
  url_file <- "https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data-geo/data-national/RIVM_NL_national.csv"
  
  raw_data <- try(read_csv(url_file))
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read Dutch data at ", url_file))
    return(NULL)
  }
  
  longData <- raw_data %>%
    dplyr::select(date = Datum, data_type = Type, value = Aantal) %>%
    mutate(value = replace_na(value, 0)) %>% 
    mutate(value = if_else(value < 0, 0, value)) %>% 
    mutate(
      data_type = recode(data_type,
                         "Totaal" = "confirmed",
                         "Ziekenhuisopname" = "hospitalized",
                         "Overleden" = "deaths"),
      countryIso3 = "NLD",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "RIVM")
  
  return(longData)
  
}

##### EXCESS Death NL
# install.packages("cbsodataR")
# The netherlands has developed an R package
# to access their central statistics data
# https://www.cbs.nl/en-gb/our-services/open-data/statline-as-open-data/quick-start-guide
#library("cbsodataR")

getDeathNLD <- function() {
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

getRawExcessDeathNLD <- function(startAt = as.Date("2020-02-20")) {
  
  relevant_weeks <- sprintf("%02d", seq(isoweek(startAt), isoweek(Sys.Date()) - 2))
  
  raw_data <- getDeathNLD()
  
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

getExcessDeathNLD <- function(startAt = as.Date("2020-02-20")) {
  excess_death <- suppressWarnings(getRawExcessDeathNLD(startAt))
  
  longData <- excess_death %>%
    dplyr::select(date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      countryIso3 = "NLD",
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "CBS") %>%
    mutate(value = ifelse(value < 0, 0, value))
  
  return(longData)
}

getDataNLD <- function() {
  caseData <- getCaseDataNLD()
  excessDeath <- NULL#getExcessDeathNLD()
  allData <- bind_rows(caseData, excessDeath)
  return(allData)
}

## Spain

getCaseDataESP <- function(){
  #url_csv_file <- "https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv"
  url_csv_file <- "https://cnecovid.isciii.es/covid19/resources/casos_diagnostico_ccaa.csv"
  
  raw_data <- try(read_csv(url_csv_file))
  
  confirmed_onsets <- raw_data %>% 
    dplyr::select(ccaa_iso, fecha, num_casos) %>% 
    rename(date = fecha) %>% 
    group_by(date) %>% 
    dplyr::summarise(value = sum(num_casos), .groups = "drop") %>% 
    arrange(date) %>% 
    mutate(region = "ESP", 
           countryIso3 = "ESP",
           source = "RENAVE",
           date_type = "onset",
           local_infection = TRUE,
           data_type = "confirmed")
  
  return(confirmed_onsets)
}


getDataESP <- function(tempFile = NULL, tReload = 15) {
  #deathData <- getDataECDC(countries = "ESP", tempFileName = tempFile, tReload = tReload) %>%  filter(data_type == "death")
  deathData <- getDataOWID(countries = "ESP", tempFileName = tempFile, tReload = tReload) %>%  filter(data_type == "deaths")
  caseData <- getCaseDataESP()
  excessDeath <- NULL
  allData <- bind_rows(caseData, deathData, excessDeath)
  return(allData)
}





##### Qatar #####

getDataQAT <- function(){
  url <- "https://www.data.gov.qa/explore/dataset/covid-19-cases-in-qatar/download/?format=csv&timezone=Europe/Berlin&lang=en&use_labels_for_header=true&csv_separator=%3B"
  raw_data <- try(read_delim(url, delim = ";"))
  
  all_data <- raw_data %>% 
    transmute(date = Date,
              confirmed = `Number of New Positive Cases in Last 24 Hrs`,
              deaths = `Number of New Deaths in Last 24 Hrs`,
              hospitalized = `Number of New Acute Hospital Admissions in Last 24 Hrs`) %>% 
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(confirmed = 0,
                         deaths = 0,
                         hospitalized = 0)) %>% 
    pivot_longer(cols = c(confirmed, deaths, hospitalized), names_to = "data_type") %>% 
    mutate(countryIso3 = "QAT",
           local_infection = TRUE,
           date_type = "report",
           region = "QAT",
           source = "QMPH")
  
  return(all_data)
}

##### South Africa #####

getConfirmedCasesZAF <- function(
  url = paste0("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/",
               "covid19za_provincial_cumulative_timeline_confirmed.csv")) {
  
  raw_data <- try( read_csv(url,
                            col_types = cols(
                              .default = col_double(),
                              date = col_date(format = "%d-%m-%Y"),
                              source = col_character())) )
  
  
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read ZAF data at ", url))
    return(NULL)
  }
  
  confirmedCases <- raw_data  %>%
    dplyr::select(-YYYYMMDD, -source) %>%
    pivot_longer(cols = EC:total, names_to = "region") %>%
    arrange(region, date) %>%
    fill(value, .direction = "down") %>%
    group_by(region) %>%
    transmute(
      date = date,
      countryIso3 = "ZAF",
      region = recode(region,
                      EC = "Eastern Cape",
                      FS = "Free State",
                      GP = "Gauteng",
                      KZN = "KwaZulu-Natal",
                      LP = "Limpopo",
                      MP = "Mpumalanga",
                      NC = "Northern Cape",
                      NW = "North West",
                      WC = "Western Cape",
                      UNKNOWN = "Unknown",
                      total = "ZAF"),
      data_type = "confirmed",
      value = diff(c(0, value)),
      date_type = "report",
      local_infection = TRUE,
      source = "DSFSI"
    )
  # replace negative numbers by 0
  confirmedCases$value[confirmedCases$value < 0] <- 0
  
  return(confirmedCases)
}

getDeathsZAF <- function(
  url = paste0("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/",
               "covid19za_provincial_cumulative_timeline_deaths.csv")) {
  
  raw_data <- try( read_csv(url,
                            col_types = cols(
                              .default = col_double(),
                              date = col_date(format = "%d-%m-%Y"),
                              source = col_character())) )
  
  if ("try-error" %in% class(raw_data)) {
    warning(str_c("Couldn't read ZAF data at ", url))
    return(NULL)
  }
  
  deaths <- raw_data  %>%
    dplyr::select(-YYYYMMDD, -source) %>%
    pivot_longer(cols = EC:total, names_to = "region") %>%
    arrange(region, date) %>%
    fill(value, .direction = "down") %>%
    group_by(region) %>%
    transmute(
      date = date,
      countryIso3 = "ZAF",
      region = recode(region,
                      EC = "Eastern Cape",
                      FS = "Free State",
                      GP = "Gauteng",
                      KZN = "KwaZulu-Natal",
                      LP = "Limpopo",
                      MP = "Mpumalanga",
                      NC = "Northern Cape",
                      NW = "North West",
                      WC = "Western Cape",
                      UNKNOWN = "Unknown",
                      total = "ZAF"),
      data_type = "deaths",
      value = diff(c(0, value)),
      date_type = "report",
      local_infection = TRUE,
      source = "DSFSI"
    )
  # replace negative numbers by 0
  deaths$value[deaths$value < 0] <- 0
  
  return(deaths)
}

getDataZAF <- function() {
  confirmedCases <- getConfirmedCasesZAF()
  deaths <- getDeathsZAF()
  allData <- bind_rows(confirmedCases, deaths)
  return(allData)
}


##### Switzerland  & Liechtenstein #######
sumGreaterRegions <- function(chData) {
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
    filter(countryIso3 == "CHE") %>%
    left_join(greaterRegions, by = "region") %>%
    ungroup() %>%
    mutate(region = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, region, source, data_type, date_type, local_infection) %>%
    dplyr::summarize(
      value = sum(value),
      countryIso3 = "CHE",
      .groups = "keep")
  return(greaterRegionsData)
}

getDataCHEBAG <- function(path = here::here("app/data/CHE"), country = "CHE", filename = "incidence_data_CHE.csv") {
  filePath <- file.path(path, filename)
  bagData <- read_csv(filePath,
                      col_types = cols(
                        date = col_date(format = ""),
                        region = col_character(),
                        countryIso3 = col_character(),
                        source = col_character(),
                        data_type = col_character(),
                        value = col_double(),
                        positiveTests = col_double(),
                        negativeTests = col_double(),
                        totalTests = col_double(),
                        testPositivity = col_double(),
                        date_type = col_character(),
                        local_infection = col_logical())) %>%
    filter(countryIso3 == country)
  
  bagDataGreaterRegions <- sumGreaterRegions(filter(bagData, region != "CHE"))
  
  bagDataAll <- bind_rows(bagData, bagDataGreaterRegions)
  
  return(bagDataAll)
}

getDataCHEexcessDeath <- function(startAt = as.Date("2020-02-20")) {
  # urlPastData = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12607335/master"
  # pastData <- read_delim(urlPastData, delim = ";", comment = "#")
  
  url2020 <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/13047388/master"
  data2020 <- try(
    read_delim(url2020, delim = ";",
               col_types = cols(
                 .default = col_double(),
                 Ending = col_character(),
                 Age = col_character()
               )
    )
  )
  
  if ("try-error" %in% class(data2020)) {
    warning(str_c("Couldn't read swiss excess death data at ", url))
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
    mutate(
      countryIso3 = "CHE",
      date_type = "report",
      local_infection = TRUE,
      region = "CHE",
      source = "BFS") %>%
    mutate(value = ifelse(value < 0, 0, value))
  
  return(longData)
}

getDataCHE <- function(data_path) {
  bagData <- getDataCHEBAG(path = data_path, country = "CHE")
  swissExcessDeath <- NULL #getDataCHEexcessDeath(startAt = as.Date("2020-02-20"))
  swissData <- bind_rows(
    bagData,
    swissExcessDeath) %>%
    mutate(
      region = recode(region, "CH" = "CHE")) %>%
    ungroup()
  return(swissData)
}

getDataLIE <- function(data_path) {
  bagDataLIE <- getDataCHEBAG(path = data_path, country = "LIE") %>%
    mutate(
      region = recode(region, "FL" = "LIE"))
  return(bagDataLIE)
}

getVaccinationDataCHE <- function() {
  urlfile <- jsonlite::fromJSON("https://www.covid19.admin.ch/api/data/context")

  # load total vaccinations
  totalVacc <- read_csv(
    urlfile$sources$individual$csv$vaccDosesAdministered,
    col_types = cols_only(
      date = col_date(format = ""),
      geoRegion = col_character(),
      entries = col_double(),
      pop = col_double(),
      sumTotal = col_double(),
      per100PersonsTotal = col_double()
    )
  )

  if ("try-error" %in% class(totalVacc)) {
    warning(str_c(
      "couldn't get total Vaccination data from ",
      urlfile$sources$individual$csv$vaccDosesAdministered, "."
    ))
    return(NULL)
  }

  # load full vaccinations
  totalFullyVacc <- read_csv(
    urlfile$sources$individual$csv$fullyVaccPersons,
    col_types = cols_only(
      date = col_date(format = ""),
      geoRegion = col_character(),
      entries = col_double(),
      pop = col_double(),
      sumTotal = col_double(),
      per100PersonsTotal = col_double()
    )
  )

  if ("try-error" %in% class(totalFullyVacc)) {
    warning(str_c(
      "couldn't get total Vaccination data from ",
      urlfile$sources$individual$csv$vaccDosesAdministered, "."
    ))
    return(NULL)
  }

  # calculate vaccinations for greater Regions
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

  totalVaccGrRegions <- totalVacc %>%
    filter(geoRegion %in% unique(greaterRegions$region)) %>%
    left_join(greaterRegions, by = c("geoRegion" = "region")) %>%
    ungroup() %>%
    mutate(geoRegion = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, geoRegion) %>%
    dplyr::summarize(
      entries = sum(entries),
      pop = sum(pop),
      sumTotal = sum(sumTotal),
      .groups = "drop") %>%
    mutate(per100PersonsTotal = (sumTotal / pop) * 100)

  totalFullyVaccGrRegions <- totalFullyVacc %>%
    filter(geoRegion %in% unique(greaterRegions$region)) %>%
    left_join(greaterRegions, by = c("geoRegion" = "region")) %>%
    ungroup() %>%
    mutate(geoRegion = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, geoRegion) %>%
    dplyr::summarize(
      entries = sum(entries),
      pop = sum(pop),
      sumTotal = sum(sumTotal),
      .groups = "drop") %>%
    mutate(per100PersonsTotal = (sumTotal / pop) * 100)

  totalVaccLong <- bind_rows(totalVacc, totalVaccGrRegions) %>%
    transmute(
      countryIso3 = if_else(geoRegion == "FL", "LIE", "CHE"),
      region = recode(geoRegion, CH = "CHE", FL = "LIE"),
      date = date,
      daily_vaccinations_raw = entries,
      total_vaccinations = sumTotal,
      total_vaccinations_per_hundred = per100PersonsTotal
    ) %>%
    pivot_longer(cols = daily_vaccinations_raw:total_vaccinations_per_hundred, names_to = "data_type") %>%
    mutate(source = "FOPH")

  totalFullyVaccLong <- bind_rows(totalFullyVacc, totalFullyVaccGrRegions) %>%
    transmute(
      countryIso3 = if_else(geoRegion == "FL", "LIE", "CHE"),
      region = recode(geoRegion, CH = "CHE", FL = "LIE"),
      date = date,
      people_fully_vaccinated = sumTotal,
      people_fully_vaccinated_per_hundred = per100PersonsTotal
    ) %>%
    pivot_longer(cols = people_fully_vaccinated:people_fully_vaccinated_per_hundred, names_to = "data_type") %>%
    mutate(source = "FOPH")

  longData <- bind_rows(totalVaccLong, totalFullyVaccLong) %>%
    arrange(countryIso3, region, data_type, date) %>%
    filter(region != "CHFL") %>%
    filter(!is.na(value))
}

##### United Kingdom #####

getRawExcessDeathGBR <- function(startAt = as.Date("2020-02-20"), path_to_data = "../data/UK") {
  
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

getExcessDeathGBR <- function(startAt = as.Date("2020-02-20"), path_to_data = "../data/UK") {
  excess_death <- getRawExcessDeathGBR(startAt, path_to_data)
  
  longData <- excess_death %>%
    dplyr::select(date, deaths = covid_deaths, excess_deaths) %>%
    pivot_longer(cols = c(deaths, excess_deaths), names_to = "data_type") %>%
    mutate(
      countryIso3 = "GBR", 
      date_type = "report",
      local_infection = TRUE,
      region = countryIso3,
      source = "ONS",
      value = ifelse(value < 0, 0, value))
  
  return(longData)
}

getDataGBR <- function(tempFile = NULL, HMDtemp = NULL, tReload = 15) {
  url <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation&structure=%7B%22areaType%22:%22areaType%22,",
                "%22areaName%22:%22areaName%22,%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,",
                "%22newDeaths28DaysByDeathDate%22:%22newDeaths28DaysByDeathDate%22,",
                "%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22%7D&format=csv")
  
  rawData <- try(read_csv(url))
  if ("try-error" %in% class(rawData)) {
    warning(str_c("Couldn't read UK data at ", url))
    return(NULL)
  }
  
  #for now we aggregate all the UK nations back together
  longData <- rawData %>%
    group_by(date, areaName) %>% 
    summarise(confirmed = sum(newCasesBySpecimenDate),
              hospitalized = sum(newAdmissions),
              deaths = sum(newDeaths28DaysByDeathDate),
              .groups = "drop") %>% 
    pivot_longer(cols = c("confirmed", "hospitalized", "deaths"),
                 values_to = "value",
                 names_to = "data_type") %>% 
    filter(!is.na(value)) %>% 
    rename(region = areaName) %>% 
    mutate( local_infection = TRUE,
            countryIso3 = "GBR",
            source = "Gov.UK",
            date_type = "report") %>% 
    arrange(region, data_type, date) 
  
  UK_data <- longData %>% 
    group_by(date, data_type, date_type, local_infection, countryIso3, source) %>% 
    summarise(value = sum(value),
              .groups= "drop") %>% 
    mutate(region = "GBR")
  
  
  excessDeath <- NULL#getExcessDeathHMD(countries = "GBR", tempFileName = HMDtemp, tReload = tReload)
  allData <- bind_rows(longData, UK_data, excessDeath)
  return(allData)
}





##### generic functions #####

# currently implemented
# Austria, Belgium*, France*, Germany, Italy*, Netherlands*, Spain, Switzerland*, Sweden, UK*

getDataGeneric <- function(countries, tempFile = NULL, HMDtemp = NULL, tReload = 300) {
  caseData <- getDataOWID(countries = countries, tempFileName = tempFile, tReload = tReload)
  excessDeath <- NULL #getExcessDeathHMD(countries = countries, tempFileName = HMDtemp, tReload = tReload)
  allData <- bind_rows(caseData, excessDeath)
  return(allData)
}

getCountryData <- function(countries, tempFile = NULL, HMDtemp = NULL, tReload = 300, v = TRUE) {
  
  allDataList <- list()
  
  for (i in seq_len(length(countries))) {
    if (v) {
      cat(as.character(Sys.time()), " | ", str_c(countries[i], ": getting data... "))
    }
    if (countries[i] == "BEL") {
      allDataList[[i]] <- getDataBEL()
    } else if (countries[i] == "CHE") {
      allDataList[[i]] <- getDataCHE(data_path = here::here("app/data/CHE"))
    } else if (countries[i] == "CZE") {
      allDataList[[i]] <- getDataCZE()
    } else if (countries[i] == "DEU") {
      allDataList[[i]] <- getDataDEU(tempFile = tempFile, tReload = tReload)
    } else if (countries[i] == "ESP") {
      allDataList[[i]] <- getDataESP()
    } else if (countries[i] == "EST") {
      allDataList[[i]] <- getDataEST()
    } else if (countries[i] == "FRA") {
      allDataList[[i]] <- getDataFRA()
    } else if (countries[i] == "GBR") {
      allDataList[[i]] <- getDataGBR(tempFile = tempFile, HMDtemp = HMDtemp, tReload = tReload)
    } else if (countries[i] == "HKG") {
      allDataList[[i]] <- getDataHKG(data_path = here::here("app/data/HKG"))
    } else if (countries[i] == "IRL") {
      allDataList[[i]] <- getDataIRL()
    } else if (countries[i] == "ITA") {
      allDataList[[i]] <- getDataITA(tempFile = tempFile)
    } else if (countries[i] == "LIE") {
      allDataList[[i]] <- getDataLIE(data_path = here::here("app/data/CHE"))
    } else if (countries[i] == "LVA") {
      allDataList[[i]] <- getDataLVA()
    } else if (countries[i] == "NLD") {
      allDataList[[i]] <- getDataNLD()
    } else if (countries[i] == "QAT") {
      allDataList[[i]] <- getDataQAT()
    } else if (countries[i] == "ZAF") {
      allDataList[[i]] <- getDataZAF()
    } else {
      allDataList[[i]] <- getDataGeneric(countries[i], tempFile = tempFile, HMDtemp = HMDtemp, tReload = tReload)
    }
    if (v) {
      cat("done. \n")
    }
  }
  
  allData <- bind_rows(allDataList) %>%
    mutate(
      data_type = factor(data_type,
                         levels = c("confirmed",
                                    "hospitalized",
                                    "deaths",
                                    "excess_deaths",
                                    "Confirmed cases / tests"),
                         labels = c("Confirmed cases",
                                    "Hospitalized patients",
                                    "Deaths",
                                    "Excess deaths",
                                    "Confirmed cases / tests")
      )
    )
}

getCountryPopData <- function(tempFileName = here::here("app/data/pop_sizes.xls"), tReload = 15) {
  url_file <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"
  if (is.null(tempFileName)) {
    xls_path <- url_file
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(url_file, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new World Bank data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new World Bank data.")
          return(NULL)
        }
      }
    }
    xls_path <- tempFileName
  }
  
  popData <- readxl::read_xls(xls_path, sheet = 1, skip = 3) %>% 
    filter(`Indicator Name` == "Population, total") %>% 
    dplyr::select(-c(`Indicator Name`, `Indicator Code`)) %>%
    rename(country = `Country Name`,
           countryIso3 = `Country Code`) %>% 
    pivot_longer(cols = -c(country, countryIso3),
                 names_to = "year",
                 values_to = "populationSize") %>% 
    group_by(`countryIso3`) %>% 
    tidyr::fill(populationSize) %>%
    ungroup() %>% 
    filter(!is.na(populationSize),
           year == max(year)) %>% 
    dplyr::select(c(country, countryIso3, populationSize))
  return(popData)
}


#' Get Oxford Stringency Index
#'
#' fetches data for the Oxford Stringency Index from https://covidtracker.bsg.ox.ac.uk
#'
#' @param countries vector of ISO alpha-3 country codes
#' @param tempFileName wheter to save and/or use a temporary file with all data from ECDC. Useful when requesting
#'   multiple countries one after the other.
#' @param tReload if using a temporary file after how many minutes should the file be releaoded.
#' @return a tibble of the incidence data from ECDC
#' @export
#' @family fetchData
getDataOxfordStringency <- function(
  countries = NULL,
  tempFileName = NULL, tReload = 15) {

  url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

  if (is.null(tempFileName)) {
    csvPath <- url
  } else {
    fileMod <- file.mtime(tempFileName)
    fileReload <- if_else(file.exists(tempFileName), now() > fileMod + minutes(tReload), TRUE)
    if (fileReload) {
      downloadOK <- try(download.file(url, destfile = tempFileName))
      if ("try-error" %in% class(downloadOK)) {
        if (file.exists(tempFileName)) {
          warning("Couldn't fetch new Oxford Stringency data. Using data from ", fileMod)
        } else {
          warning("Couldn't fetch new Oxford Stringency data.")
          return(NULL)
        }
      }
    }
    csvPath <- tempFileName
  }

  oxfordStringencyData <- try(read_csv(csvPath, col_types = cols(
    .default = col_guess(),
    CountryName = col_character(),
    CountryCode = col_character(),
    RegionName = col_character(),
    RegionCode = col_character(),
    Date = col_date(format = "%Y%m%d"),
    M1_Wildcard = col_character()
  ))) %>%
  dplyr::rename(
    date = Date,
    countryIso3 = CountryCode,
    region = RegionCode
  )
  oxfordStringencyData$region[is.na(oxfordStringencyData$region)] <-
  oxfordStringencyData$countryIso3[is.na(oxfordStringencyData$region)]

  if ("try-error" %in% class(oxfordStringencyData)) {
    warning(str_c("couldn't get Oxford Stringency Data data from ", url, "."))
    return(NULL)
  }

  if (!is.null(countries)) {
    oxfordStringencyData <- oxfordStringencyData %>%
      filter(countryIso3 %in% countries)
  }
  return(oxfordStringencyData)
}
