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

##### ECDC #####

getDataECDC <- function(countries = NULL) {
  urlfile <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  world_data <- try(read_csv(urlfile,
    col_types = cols(
      .default = col_double(),
      dateRep = col_character(),
      countriesAndTerritories = col_character(),
      geoId = col_character(),
      countryterritoryCode = col_character(),
      continentExp = col_character()
    )) %>% mutate(dateRep = dmy(date)))

  if ("try-error" %in% class(world_data)) {
    cat("ECDC csv not available... \nTrying .xlsx ... \n")
    xlsFile <- str_c("https://www.ecdc.europa.eu/sites/default/files/documents/",
    "COVID-19-geographic-disbtribution-worldwide.xlsx")
    downloadOK <- try(download.file(xlsFile, destfile = here::here("app/data/temp", "ECDCdataTemp.xlsx")))
    if ("try-error" %in% class(downloadOK)) {
      cat("Coulnd't get new xlsx file. Trying to read older xslx file...")
    } else {
      file.rename(
        from = here::here("app/data/temp", "ECDCdataTemp.xlsx"),
        to = here::here("app/data", "ECDCdata.xlsx"))
      cat(".xlsx ok!... \n")
    }
    world_data <- try(readxl::read_excel(here::here("app/data", "ECDCdata.xlsx")) %>%
      mutate(dateRep = ymd(as.character(dateRep))))
    if ("try-error" %in% class(world_data)) {
      cat("ECDC data not available... \n")
      return(NULL)
    }
  }

  longData <- world_data %>%
    dplyr::select(
      c(date = "dateRep", country = "countriesAndTerritories", popSize = popData2019,
        confirmed = "cases", deaths = "deaths")) %>%
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>%
    mutate(
      variable = "incidence",
      country = gsub("_", " ", country),
      region = country,
      source = "ECDC") %>%
    filter(!is.na(value))

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(country %in% countries)
  }

  longData[longData$value < 0, "value"] <- 0
  return(longData)
}

##### Human Mortality Database #####

getExcessDeathHMD <- function(startAt = as.Date("2020-02-20"), countries = NULL) {
  # Human mortality database

  url <- "https://www.mortality.org/Public/STMF/Outputs/stmf.csv"
  rawData <- try(
    read_csv(url, comment = "#",
      col_types = cols(
        .default = col_double(),
        CountryCode = col_character(),
        Sex = col_character()
      )
    )
  )

  if ("try-error" %in% class(rawData)) {
    warning(str_c("couldn't get mortality data from ", url,"."))
    return(NULL)
  }

  tidy_data <- rawData %>%
    dplyr::select(country = CountryCode, year = Year,
      week = Week, sex = Sex, deaths = DTotal) %>%
    filter(sex == "b") %>%
    mutate(country = recode(country,
      AUT = "Austria", BEL = "Belgium",
      BGR = "Bulgaria", CZE = "Czech Republic",
      DEUTNP = "Germany", DNK = "Denmark", ESP = "Spain",
      FIN = "Finland", GBRTENW = "United Kingdom", ISL = "Island",
      NLD = "Netherlands", NOR = "Norway", PRT = "Portugal", SWE = "Sweden",
      USA = "USA"))

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

  if (!is.null(countries)) {
    longData <- filter(
      longData,
      country %in% countries
    )
  }

  return(longData)
}

##### Switzerland #######
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
    filter(country == "CH", region != "FL") %>%
    left_join(greaterRegions, by = "region") %>%
    ungroup() %>%
    mutate(region = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, region, source, data_type, variable) %>%
    dplyr::summarize(
      value = sum(value),
      country = "CH",
      .groups = "keep")
  return(greaterRegionsData)
}

getDataCHopenZH <- function(stopAfter = (Sys.Date() - 1)) {
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

getDataCHBAG <- function(path, filename = "incidence_data_CH.csv") {
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

getHospitalDataCH <- function(path,
  region = "CH", csvBaseName = "Hospital_cases", dataTypeSuffix="") {
  filePath <- file.path(path, str_c(csvBaseName, dataTypeSuffix, "_", region, ".csv"))

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
    warning("Swiss Hospital Data file not found. Ignoring...")
    out <- NULL
  }
  return(out)
}

getDataCHExcessDeath <- function(startAt = as.Date("2020-02-20")) {
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
    mutate(country = "Switzerland", variable = "incidence",
           region = country, source = "BFS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  return(longData)
}

getDataCH <- function(stoppingAfter = (Sys.Date() - 1), pathToHospData) {
  #openZHData <- getDataCHopenZH(stopAfter = stoppingAfter)
  bagData <- getDataCHBAG(path = pathToHospData)
  hospitalData <- getHospitalDataCH(path = pathToHospData, region = "CH", dataTypeSuffix = "")
  hospitalData_onsets <- getHospitalDataCH(path = pathToHospData, region = "CH", dataTypeSuffix = "_onsets")
  hospitalData_admissions <- getHospitalDataCH(path = pathToHospData, region = "CH", dataTypeSuffix = "_admissions")
  swissExcessDeath <- getDataCHExcessDeath(startAt = as.Date("2020-02-20"))
  swissData <- bind_rows(
    bagData,
    hospitalData,
    hospitalData_onsets,
    hospitalData_admissions,
    swissExcessDeath) %>%
    filter(variable == "incidence") %>%
    mutate(
      region = recode(region, "CH" = "Switzerland", "FL" = "Liechtenstein"),
      country = recode(country, "CH" = "Switzerland", "FL" = "Liechtenstein")) %>%
    ungroup()

  return(swissData)
}

##### Italy #######

getExcessDeathIT <- function(filePath = here::here("../covid19-additionalData/excessDeath/Excess_death_IT.csv"),
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
    mutate(variable = "incidence", country = "Italy",
           region = country, source = "Istat") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(!is.na(value))

  return(longData)
}

# WIP not functional
getITDataPCM <- function(){
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
      country = "Italy",
      region = "Italy",
      source = "PCM-DPC",
      variable = "incidence",
      confirmed = nuovi_positivi,
      deaths = diff(c(0, deceduti))
    ) %>%
    pivot_longer(cols = confirmed:deaths, names_to = "data_type", values_to = "value") %>%
    arrange(country, region, source, variable, data_type, date) %>%
    group_by(country, region, source, variable, data_type) %>%
    mutate(
      cumul = cumsum(value))

  cumulData <- data %>%
    group_by(data_type) %>%
    mutate(
      variable = "cumul",
      value = cumsum(value)) %>%
    filter(data_type == "deaths")

    return(data)
}

getDataIT <- function(){
  caseData <- getDataECDC(countries = "Italy")
  excessDeath <- getExcessDeathIT()

  allData <- bind_rows(caseData, excessDeath)

  return(allData)
}

##### France #######

getHospitalDataFR <- function() {
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
           country = "France",
           variable = "incidence",
           region = country,
           source = "SpF-DMI")

  return(longData)
}

getExcessDeathFR <- function(startAt = as.Date("2020-02-20")) {
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

getDataFR <- function(){
  caseData <- getDataECDC(countries = "France")
  excessDeath <- getExcessDeathFR()
  hospitalData <- getHospitalDataFR()

  allData <- bind_rows(caseData, excessDeath, hospitalData)

  return(allData)
}

##### Belgium #####

getHospitalDataBE <- function() {
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
    warning(str_c("Couldn't read french hospital data at ", url))
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
      country = "Belgium",
      variable = "incidence",
      region = country,
      source = "Sciensano")

  return(longData)
}

getDataBE <- function(){
  caseData <- getDataECDC(countries = "Belgium")
  excessDeath <- getExcessDeathHMD(countries = "Belgium")
  hospitalData <- getHospitalDataBE()

  allData <- bind_rows(caseData, excessDeath, hospitalData)

  return(allData)
}

##### Netherlands #####
getCaseDataNL <- function(stopAfter = Sys.Date(), startAt = as.Date("2020-02-20")) {
  baseurl <- str_c("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/",
    "rivm_NL_covid19_national_by_date/rivm_NL_covid19_national_by_date_")

  urlfile <- paste0(baseurl, stopAfter, ".csv")
  raw_data <- try(read_csv(urlfile))

  if ("try-error" %in% class(raw_data)) {
    urlfile <- paste0(baseurl, "latest.csv")
    raw_data <- read_csv(urlfile)
  }

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

getDataNL <- function(){
  caseData <- getCaseDataNL()
  excessDeath <- getExcessDeathNL()

  allData <- bind_rows(caseData, excessDeath)

  return(allData)
}

##### UK #####

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

getDataUK <- function(){
  caseData <- getDataECDC(countries = "United Kingdom")
  excessDeath <- getExcessDeathUK()

  allData <- bind_rows(caseData, excessDeath)

  return(allData)
}

##### generic functions #####

# currently implemented
# Austria, Belgium*, France*, Germany, Italy*, Netherlands*, Spain, Switzerland*, Sweden, United Kingdom*

getDataGeneric <- function(countries){
  caseData <- getDataECDC(countries = countries)
  excessDeath <- getExcessDeathHMD(countries = countries)

  allData <- bind_rows(caseData, excessDeath)

  return(allData)
}

getCountryData <- function(countries) {

  allDataList <- list()

  for (i in seq_len(length(countries))) {
    if (countries[i] == "Belgium") {
      allDataList[[i]] <- getDataBE()
    } else if (countries[i] == "France") {
      allDataList[[i]] <- getDataFR()
    } else if (countries[i] == "Italy") {
      allDataList[[i]] <- getDataIT()
    } else if (countries[i] == "Netherlands") {
      allDataList[[i]] <- getDataNE()
    } else if (countries[i] == "Switzerland") {
      allDataList[[i]] <- getDataCH(pathToHospData = here::here("app/data/CH"))
    } else if (countries[i] == "United Kingdom") {
      allDataList[[i]] <- getDataUK()
    } else {
      allDataList[[i]] <- getDataGeneric(countries[i])
    }
  }

  allData <- bind_rows(allDataList)

}
