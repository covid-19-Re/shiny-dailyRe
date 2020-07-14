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
  cumulData <- reshape2::melt(cumulData, id.vars = nameDateCol)
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
    warning(str_c("couldn't get ECDC data from ", url,"."))
    return(NULL)
  }

  longData <- world_data %>%
    select(
      date = "dateRep",
      countryIso3 = "countryterritoryCode",
      region = "countryterritoryCode",
      confirmed = "cases", deaths = "deaths") %>%
    pivot_longer(cols = c(confirmed, deaths), names_to = "data_type") %>%
    mutate(
      variable = "incidence",
      source = "ECDC") %>%
    filter(!is.na(value))

  if (!is.null(countries)) {
    longData <- longData %>%
      filter(countryIso3 %in% countries)
  }

  longData[longData$value < 0, "value"] <- 0
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
    select(countryIso3 = CountryCode, year = Year,
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
    select(countryIso3, region, year, week, deaths) %>%
    left_join(past_data, by = c("countryIso3", "week")) %>%
    mutate(
      excess_deaths = ceiling(deaths - avg_deaths),
      date = ymd(
        parse_date_time(
          paste(year, week, "Mon", sep = "/"), "Y/W/a",
          locale = "en_GB.UTF-8"
        ), locale = "en_GB.UTF-8")) %>%
    select(-year, -week)

  longData <- excess_death %>%
    select(date, countryIso3, region, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      variable = "incidence",
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
    filter(countryIso3 == "CHE") %>%
    left_join(greaterRegions, by = "region") %>%
    ungroup() %>%
    mutate(region = greaterRegion) %>%
    dplyr::select(-greaterRegion) %>%
    group_by(date, region, source, data_type, variable) %>%
    dplyr::summarize(
      value = sum(value),
      countryIso3 = "CHE",
      .groups = "keep")
  return(greaterRegionsData)
}

getDataCHEopenZH <- function(stopAfter = (Sys.Date() - 1)) {
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
  mutate(countryIso3 = if_else(region != "FL", "CHE", "LIE")) %>%
  filter(date <= stopAfter)

  openZHcantons <- openZHraw  %>%
    group_by(data_type, region) %>%
    nest() %>%
    mutate(
      dataCurated = map(data, curateLongTimeSeries, isIncidenceData = FALSE),
    ) %>%
    select(-data) %>%
    unnest(cols = "dataCurated")

  openZHsum <- openZHraw %>%
    arrange(region, data_type, variable, date) %>%
    group_by(region, data_type) %>%
    fill(value, .direction = "down") %>%
    filter(!is.na(value))

  openZHswitzerland <- openZHsum %>%
    filter(countryIso3 == "CHE") %>%
    group_by(date, countryIso3, source, data_type, variable) %>%
    summarize(
      value = sum(value),
      region = "CHE",
      .groups = "keep")

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
  dplyr::select(date, region, countryIso3, source, data_type, value, variable) %>%
  arrange(countryIso3, region, data_type, variable, date)

  return(openZHdata)
}

getDataCHEBAG <- function(path, filename = "incidence_data_CH.csv") {
  filePath <- file.path(path, filename)
  bagData <- read_csv(filePath,
    col_types = cols(
      date = col_date(format = ""),
      region = col_character(),
      countryIso3 = col_character(),
      source = col_character(),
      data_type = col_character(),
      value = col_double(),
      variable = col_character()))

  bagDataGreaterRegions <- sumGreaterRegions(filter(bagData, region != "CHE"))

  bagDataAll <- bind_rows(bagData, bagDataGreaterRegions)

  return(bagDataAll)
}

getHospitalDataCHE <- function(path,
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
      mutate(
        countryIso3 = recode(as.character(region), "CH" = "CHE"),
        region = recode(as.character(region), "CH" = "CHE")) %>%
      select(-country)
  } else {
    warning("Swiss Hospital Data file not found. Ignoring...")
    out <- NULL
  }
  return(out)
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
      variable = "incidence",
      region = "CHE",
      source = "BFS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  return(longData)
}

getDataCHE <- function(stoppingAfter = (Sys.Date() - 1), pathToHospData) {
  #openZHData <- getDataCHopenZH(stopAfter = stoppingAfter)
  bagData <- getDataCHEBAG(path = pathToHospData)
  hospitalData <- getHospitalDataCHE(path = pathToHospData, region = "CH", dataTypeSuffix = "")
  hospitalData_onsets <- getHospitalDataCHE(path = pathToHospData, region = "CH", dataTypeSuffix = "_onsets")
  hospitalData_admissions <- getHospitalDataCHE(path = pathToHospData, region = "CH", dataTypeSuffix = "_admissions")
  swissExcessDeath <- getDataCHEexcessDeath(startAt = as.Date("2020-02-20"))
  swissData <- bind_rows(
    bagData,
    hospitalData,
    hospitalData_onsets,
    hospitalData_admissions,
    swissExcessDeath) %>%
    filter(variable == "incidence") %>%
    mutate(
      region = recode(region, "CH" = "CHE", "FL" = "LIE")) %>%
    ungroup()

  return(swissData)
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
      variable = "incidence",
      countryIso3 = "ITA",
      region = "ITA",
      source = "Istat") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(!is.na(value))

  return(longData)
}

# WIP not functional
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
      country = "ITA",
      region = "ITA",
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

getDataITA <- function(ECDCtemp = NULL, tReload = 15) {
  caseData <- getDataECDC(countries = "ITA", tempFileName = ECDCtemp, tReload = tReload)
  excessDeath <- getExcessDeathITA()

  allData <- bind_rows(caseData, excessDeath)

  return(allData)
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
           variable = "incidence",
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
    select(date = end_date, excess_deaths) %>%
    pivot_longer(cols = excess_deaths, names_to = "data_type") %>%
    mutate(
      countryIso3 = "FRA",
      variable = "incidence",
      region = countryIso3,
      source = "Economist") %>%
    mutate(value = ifelse(value < 0, 0, value)) %>%
    filter(date > startAt)

  return(longData)
}

getDataFRA <- function(ECDCtemp = NULL, tReload = 15){
  caseData <- getDataECDC(countries = "FRA", tempFileName = ECDCtemp, tReload = tReload)
  excessDeath <- getExcessDeathFRA()
  hospitalData <- getHospitalDataFRA()

  allData <- bind_rows(caseData, excessDeath, hospitalData)

  return(allData)
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
      variable = "incidence",
      region = countryIso3,
      source = "Sciensano")

  return(longData)
}

getDataBEL <- function(ECDCtemp = NULL, HMDtemp = NULL, tReload = 15) {
  caseData <- getDataECDC(countries = "BEL", tempFileName = ECDCtemp, tReload = tReload)
  excessDeath <- getExcessDeathHMD(countries = "BEL", tempFileName = HMDtemp, tReload = tReload)
  hospitalData <- getHospitalDataBEL()
  allData <- bind_rows(caseData, excessDeath, hospitalData)
  return(allData)
}

##### Netherlands #####
getCaseDataNLD <- function(stopAfter = Sys.Date(), startAt = as.Date("2020-02-20")) {
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
      countryIso3 = "NLD",
      variable = "incidence",
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
      variable = "incidence",
      region = countryIso3,
      source = "CBS") %>%
    mutate(value = ifelse(value < 0, 0, value))

  return(longData)
}

getDataNLD <- function() {
  caseData <- getCaseDataNLD()
  excessDeath <- getExcessDeathNLD()
  allData <- bind_rows(caseData, excessDeath)
  return(allData)
}

##### GBR #####

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
      countryIso3 = "GBR", variable = "incidence",
      region = countryIso3,
      source = "ONS",
      value = ifelse(value < 0, 0, value))

  return(longData)
}

getDataGBR <- function(ECDCtemp = NULL, HMDtemp = NULL, tReload = 15) {
  caseData <- getDataECDC(countries = "GBR", tempFileName = ECDCtemp, tReload = tReload)
  excessDeath <- getExcessDeathHMD(countries = "GBR", tempFileName = HMDtemp, tReload = tReload)
  allData <- bind_rows(caseData, excessDeath)
  return(allData)
}

##### South Africa #####

getConfirmedCasesZAF <- function(
  url = paste0("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/",
  "covid19za_provincial_cumulative_timeline_confirmed.csv")) {
    confirmedCases <- read_csv(url,
        col_types = cols(
          .default = col_double(),
          date = col_date(format = "%d-%m-%Y"),
          source = col_character())) %>%
      select(-YYYYMMDD, -source) %>%
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
        variable = "incidence",
        source = "DSFSI"
      )
    # replace negative numbers by 0
    confirmedCases$value[confirmedCases$value < 0] <- 0

    return(confirmedCases)
  }

getDeathsZAF <- function(
  url = paste0("https://raw.githubusercontent.com/dsfsi/covid19za/master/data/",
  "covid19za_provincial_cumulative_timeline_deaths.csv")) {
    deaths <- read_csv(url,
        col_types = cols(
          .default = col_double(),
          date = col_date(format = "%d-%m-%Y"),
          source = col_character())) %>%
      select(-YYYYMMDD, -source) %>%
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
        variable = "incidence",
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

##### generic functions #####

# currently implemented
# Austria, Belgium*, France*, Germany, Italy*, Netherlands*, Spain, Switzerland*, Sweden, UK*

getDataGeneric <- function(countries, ECDCtemp = NULL, HMDtemp = NULL, tReload = 15) {
  caseData <- getDataECDC(countries = countries, tempFileName = ECDCtemp, tReload = tReload)
  excessDeath <- getExcessDeathHMD(countries = countries, tempFileName = HMDtemp, tReload = tReload)
  allData <- bind_rows(caseData, excessDeath)
  return(allData)
}

getCountryData <- function(countries, ECDCtemp = NULL, HMDtemp = NULL, tReload = 15, v = TRUE) {

  allDataList <- list()

  for (i in seq_len(length(countries))) {
    if (v) {
      cat(countries[i], ": getting data... ")
    }
    if (countries[i] == "BEL") {
      allDataList[[i]] <- getDataBEL(ECDCtemp = ECDCtemp, HMDtemp = HMDtemp, tReload = tReload)
    } else if (countries[i] == "FRA") {
      allDataList[[i]] <- getDataFRA(ECDCtemp = ECDCtemp, tReload = tReload)
    } else if (countries[i] == "ITA") {
      allDataList[[i]] <- getDataITA(ECDCtemp = ECDCtemp)
    } else if (countries[i] == "NLD") {
      allDataList[[i]] <- getDataNLD()
    } else if (countries[i] == "CHE") {
      allDataList[[i]] <- getDataCHE(pathToHospData = here::here("app/data/CH"))
    } else if (countries[i] == "GBR") {
      allDataList[[i]] <- getDataGBR(ECDCtemp = ECDCtemp, HMDtemp = HMDtemp, tReload = tReload)
    } else if (countries[i] == "ZAF") {
      allDataList[[i]] <- getDataZAF()
    } else {
      allDataList[[i]] <- getDataGeneric(countries[i], ECDCtemp = ECDCtemp, HMDtemp = HMDtemp, tReload = tReload)
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
                  "hospitalized_onsets",
                  "hospitalized_admissions",
                  "deaths",
                  "excess_deaths"),
        labels = c("Confirmed cases",
                  "Hospitalized patients",
                  "Hospitalized patients - onset",
                  "Hospitalized patients - admission",
                  "Deaths",
                  "Excess deaths")
      )
    ) %>%
    filter(variable == "incidence")
}

getCountryPopData <- function() {
  wbdataJSON <- jsonlite::fromJSON(
    str_c("http://api.worldbank.org/v2/country/all/indicator/",
      "SP.POP.TOTL?MRV=1&format=json&per_page=300"
    ),
    flatten = TRUE)

  wbdata <- wbdataJSON[[2]] %>%
    as_tibble() %>%
    select(
      countryIso2 = country.id,
      countryIso3 = countryiso3code,
      country = country.value,
      year = date,
      populationSize = value
    )

  return(wbdata)
}
