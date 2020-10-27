# only show package messages when in interactive mode to clean up error logs
if (interactive()) {
  library("lubridate")
  library("fitdistrplus")
  library("EpiEstim")
  library("cbsodataR")
  library("readxl")
  library("here")
  library("tidyverse")
} else {
  suppressPackageStartupMessages({
    library("lubridate")
    library("fitdistrplus")
    library("EpiEstim")
    library("cbsodataR")
    library("readxl")
    library("here")
    library("tidyverse")
  })
}

source(here::here("app/otherScripts/1_utils_getRawData.R"))
source(here::here("app/otherScripts/utils.R"))

args <- commandArgs(trailingOnly = TRUE)
# testing
if (length(args) == 0) {
  args <- c("CHE")
  warning(str_c("Testing mode!! Country: ", args))
}
names(args) <- "country"

# Fetch Population Data (do once)
popDataPath <- here::here("app", "data", "popData.rds")

popDataWorldBank <- getCountryPopData(here::here("app/data/temp/pop_sizes.xls"), 300) %>%
  filter(!(countryIso3 %in% c("LIE", "CHE"))) %>%
  mutate(region = countryIso3)
popDataCH <- read_csv(
  file = here::here("app/data/additionalPopSizes.csv"),
  col_types = cols(
    .default = col_character(),
    populationSize = col_double()
  )
)

popData <- bind_rows(popDataWorldBank, popDataCH) %>%
  dplyr::select(country, countryIso3, region, populationSize) %>%
  filter(!is.na(countryIso3))
saveRDS(popData, file = popDataPath)

basePath <- here::here("app", "data", "countryData")
if (!dir.exists(basePath)) {
  dir.create(basePath)
}

# fetch stringency data
oxfordStringency <- getDataOxfordStringency(countries = args["country"],
                                            tempFileName = here::here("app/data/temp/oxfordStringency.csv"), tReload = 300)

# Fetch Country Data
countryData <- getCountryData(
  args["country"],
  ECDCtemp = here::here("app/data/temp/ECDCdata.csv"),
  HMDtemp = here::here("app/data/temp/HMDdata.csv"),
  tReload = 30) %>%
  left_join(
    popData,
    by = c("countryIso3", "region")
  ) %>%
  bind_rows(
    mutate(oxfordStringency, date_type = if_else(args["country"] == "CHE", "report_plotting", "report"))
  )

if (dim(countryData)[1] > 0) {
  # check for changes in country data
  countryDataPath <- file.path(basePath, str_c(args["country"], "-Data.rds"))
  if (file.exists(countryDataPath)) {
    countryDataOld <- readRDS(countryDataPath)
    # if new data is null, keep old data (can happen because of error in reading new data)
    if (is.null(countryData)) {
      countryData <- countryDataOld
    }
    dataUnchanged <- all.equal(countryData, countryDataOld)
  } else {
    dataUnchanged <- FALSE
  }
  
  if (!is.null(countryData)) {
    updateDataPath <- here::here("app", "data", "temp", "updateDataTemp.rds")
    if (file.exists(updateDataPath)) {
      updateData <- readRDS(updateDataPath)
    } else {
      updateData <- list()
    }
    
    # save updated data
    if (!isTRUE(dataUnchanged)) {
      saveRDS(countryData, file = countryDataPath)
    }
    
    # fix because swiss data contains data for two countries (CHE & LIE)
    for (i in unique(countryData$countryIso3)) {
      updateData[[i]] <- countryData %>%
        filter(countryIso3 == i) %>%
        group_by(countryIso3, country, region, source, data_type) %>%
        dplyr::summarize(lastData = max(date), .groups = "keep") %>%
        mutate(
          lastChanged = file.mtime(countryDataPath),
          lastChecked = Sys.time())
    }
    
    saveRDS(updateData, updateDataPath)
  }
  
  cleanEnv(keepObjects = c("basePath", "countryData", "dataUnchanged", "args", "popData"))
  
  # calculate Re
  # only if (data has changed OR forceUpdate.txt exists) AND countryData is not null
  condition <- (!isTRUE(dataUnchanged) | file.exists(here::here("app", "data", "forceUpdate.txt"))) &
    !is.null(countryData)
  
  if (condition) {
    cat(str_c("\n", args["country"], ": New data available. Calculating Re ...\n"))
    # get Infection Incidence
    # load functions
    source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))
    # load parameter
    source(here::here("app/otherScripts/2_params_InfectionIncidencePars.R"))
    # load empirical delays
    delays_data_path <- here::here("app", "data", "all_delays.csv")
    delays_onset_to_count <- read_csv(delays_data_path,
                                      col_types = cols(
                                        data_type = col_character(),
                                        onset_date = col_date(format = ""),
                                        count_date = col_date(format = ""),
                                        delay = col_number()))
    # constant delay distribution
    constant_delay_distributions <- list()
    for (type_i in unique(names(shape_onset_to_count))) {
      m <- get_vector_constant_waiting_time_distr(
        shape_incubation,
        scale_incubation,
        shape_onset_to_count[[type_i]],
        scale_onset_to_count[[type_i]])
      
      constant_delay_distributions <- c(constant_delay_distributions, list(m))
    }
    names(constant_delay_distributions) <- unique(names(shape_onset_to_count))
    
    constant_delay_symptom_to_report_distributions <- list()
    for (type_i in unique(names(shape_onset_to_count))) {
      m <- get_vector_constant_waiting_time_distr(
        0,
        0,
        shape_onset_to_count[[type_i]],
        scale_onset_to_count[[type_i]])
      
      constant_delay_symptom_to_report_distributions <- c(constant_delay_symptom_to_report_distributions, list(m))
    }
    names(constant_delay_symptom_to_report_distributions) <- paste0('Onset to ',  unique(names(shape_onset_to_count)))
    
    constant_delay_distributions <- c(constant_delay_distributions, constant_delay_symptom_to_report_distributions)
    
    # filter out regions with too few cases for estimation
    countryData <- countryData %>%
      filterRegions(thresholdConfirmedCases = 500)
    # remove Oxford Stringenxy Index for Re calculation
    countryData <- countryData %>%
      filter(data_type != "Stringency Index")
    
    # filter out data_types with 0 total cases
    data_type0 <- countryData %>%
      group_by(data_type) %>%
      summarize(total = sum(value), .groups = "drop") %>%
      filter(total == 0) %>%
      .$data_type
    
    countryData <- filter(countryData, !(data_type %in% data_type0))
    # country specific data filtering
    if (args["country"] == "ESP") {
      countryData <- countryData %>%
        filter(data_type != "Deaths")
      cat("ignoring data_type Deaths\n")
    } else if (args["country"] == "AUT") {
      countryData <- countryData %>%
        filter(data_type != "Deaths")
      cat("ignoring data_type Deaths\n")
    } else if (args["country"] == "CHE") {
      # no estimation for deaths per canton (too few cases)
      countryData <- countryData %>%
        filter(!(region != "CHE" & data_type == "Deaths"))
      cat("ignoring data_type Deaths on regional level\n")
    }
    
    if (nrow(countryData) > 0) {
      
      countryData <- countryData %>%
        mutate(
          data_type = fct_drop(data_type)
        )
      
      right_truncation <- 3
      
      countryData <- countryData %>%
        group_by(country, region, source, data_type) %>%
        filter(date <= (max(date) - right_truncation)) %>%
        dplyr::select(-countryIso3, -populationSize) %>%
        ungroup()
      
      # Deconvolution
      deconvolvedData <- list()
      
      deconvolvedData[[1]] <- get_all_infection_incidence(
        countryData,
        constant_delay_distributions = constant_delay_distributions,
        onset_to_count_empirical_delays = delays_onset_to_count,
        data_types = c("Confirmed cases",
                       "Hospitalized patients",
                       "Deaths"),
        n_bootstrap = 50,
        verbose = FALSE)
      
      if (args["country"] %in% c("CHE")) {
        countryDataTests <- countryData %>%
          filter(region == args["country"], data_type == "Confirmed cases / tests")
        
        deconvolvedData[[2]] <- get_all_infection_incidence(
          countryDataTests,
          constant_delay_distributions = constant_delay_distributions,
          onset_to_count_empirical_delays = delays_onset_to_count,
          data_types = c("Confirmed cases / tests"),
          n_bootstrap = 50,
          verbose = FALSE)
      }
      
      deconvolvedCountryData <- bind_rows(deconvolvedData)
      countryDataPath <- file.path(basePath, str_c(args["country"], "-DeconvolutedData.rds"))
      if (dim(deconvolvedCountryData)[1] == 0) {
        print("no data remaining")
      } else {
        saveRDS(deconvolvedCountryData, file = countryDataPath)
        # Re Estimation
        cleanEnv(keepObjects = c("basePath", "deconvolvedCountryData", "args", "popData"))
        source(here::here("app/otherScripts/3_utils_doReEstimates.R"))
        pathToAdditionalData <- here::here("../covid19-additionalData/interventions/")
        
        interventionData <- read_csv(
          str_c(pathToAdditionalData, "interventions.csv"),
          col_types = cols(
            .default = col_character(),
            date = col_date(format = ""))
        )
        
        additionalIntervalEnds <-  read_csv(
          str_c(pathToAdditionalData, "additional_interval_ends.csv"),
          col_types = cols(
            .default = col_character(),
            date = col_date(format = ""))
        )
        
        interval_ends <- interventionData
        
        swissRegions <- deconvolvedCountryData %>%
          filter(country %in% c("Switzerland", "Liechtenstein")) %>%
          dplyr::select(region) %>%
          distinct() %>%
          .$region
        
        ### Window
        window <- 3
        
        ##TODO this all_delays could be removed because we always deconvolve
        ### Delays applied
        all_delays <- list(
          "infection_Confirmed cases" = c(Cori = 0, WallingaTeunis = -5),
          "infection_Confirmed cases / tests" = c(Cori = 0, WallingaTeunis = -5),
          "infection_Deaths" = c(Cori = 0, WallingaTeunis = -5),
          "infection_Hospitalized patients" = c(Cori = 0, WallingaTeunis = -5),
          "Confirmed cases" = c(Cori = 10, WallingaTeunis = 5),
          "Confirmed cases / tests" = c(Cori = 10, WallingaTeunis = 5),
          "Deaths" = c(Cori = 20, WallingaTeunis = 15),
          "Hospitalized patients" = c(Cori = 8, WallingaTeunis = 3),
          "infection_Excess deaths" = c(Cori = 0, WallingaTeunis = -5),
          "Excess deaths" = c(Cori = 20, WallingaTeunis = 15))
        
        truncations <- list(
          left = c(Cori = 5, WallingaTeunis = 0),
          right = c(Cori = 0, WallingaTeunis = 8))
        
        ### Run EpiEstim
        countryEstimatesRaw <- doAllReEstimations(
          deconvolvedCountryData,
          slidingWindow = window,
          methods = "Cori",
          all_delays = all_delays,
          truncations = truncations,
          interval_ends = interval_ends,
          additional_interval_ends = additionalIntervalEnds,
          swissRegions = swissRegions)
        
        countryEstimates <- as_tibble(countryEstimatesRaw) %>%
          mutate(
            data_type = factor(
              data_type,
              levels = c(
                "infection_Confirmed cases",
                "infection_Confirmed cases / tests",
                "infection_Hospitalized patients",
                "infection_Deaths",
                "infection_Excess deaths"),
              labels = c(
                "Confirmed cases",
                "Confirmed cases / tests",
                "Hospitalized patients",
                "Deaths",
                "Excess deaths"))) %>%
          pivot_wider(names_from = "variable", values_from = "value") %>%
          dplyr::group_by(date, country, region, data_type, source, estimate_type) %>%
          dplyr::summarize(
            median_R_mean = median(R_mean),
            median_R_highHPD = median(R_highHPD),
            median_R_lowHPD = median(R_lowHPD),
            .groups = "keep"
          ) %>%
          dplyr::select(country, region, source, data_type, estimate_type, date,
                        median_R_mean, median_R_highHPD, median_R_lowHPD) %>%
          arrange(country, region, source, data_type, estimate_type, date) %>%
          ungroup() %>%
          left_join(
            dplyr::select(popData, region, countryIso3),
            by = c("region")
          )
        countryDataPath <- file.path(basePath, str_c(args["country"], "-Estimates.rds"))
        saveRDS(countryEstimates, file = countryDataPath)
        # Save as .csv for data upload
        write_csv(countryEstimates,
                  path = here::here(str_c("../dailyRe-Data/", args["country"], "-estimates.csv"))
        )
      }
    } else {
      cat(str_c(args["country"], ": Not enough cases. Skipping Re calculation.\n"))
    }
    
  } else {
    cat(str_c(args["country"], ": No new data available. Skipping Re calculation.\n"))
  }
} else {
  cat(str_c(args["country"], ": No data available.\n"))
}
