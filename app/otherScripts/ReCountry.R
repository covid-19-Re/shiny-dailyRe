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

basePath <- here::here("app", "data", "countryData")
if (!dir.exists(basePath)) {
  dir.create(basePath)
}
tempPath <- here::here("app", "data", "temp")
if (!dir.exists(tempPath)) {
  dir.create(tempPath)
}

# Fetch Population Data (do once)
popDataWorldBank <- getCountryPopData(here::here(tempPath, "pop_sizes.xls"), 300) %>%
  filter(!(countryIso3 %in% c("LIE", "CHE"))) %>%
  mutate(region = countryIso3)
popDataAdditional <- read_csv(
  file = here::here("app/data/additionalPopSizes.csv"),
  col_types = cols(
    .default = col_character(),
    populationSize = col_double()
  )
)

popData <- bind_rows(popDataWorldBank, popDataAdditional) %>%
  dplyr::select(country, countryIso3, region, populationSize) %>%
  filter(!is.na(countryIso3))

# Fetch Country Data
countryData <- getCountryData(
  args["country"],
  tempFile = here::here(tempPath, "ECDCdata.csv"),
  HMDtemp = here::here(tempPath, "HMDdata.csv"),
  tReload = 300)

if (dim(countryData)[1] > 0) {
  countryData <- countryData %>%
    left_join(
      popData,
      by = c("countryIso3", "region")
    )
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
    # save updated data
    if (!isTRUE(dataUnchanged)) {
      saveRDS(countryData, file = countryDataPath)
    }
  }

  cleanEnv(keepObjects = c("basePath", "tempPath", "countryData", "dataUnchanged", "args", "popData"))

  # calculate Re
  # only if (data has changed OR forceUpdate.txt exists) AND countryData is not null
  condition <- (!isTRUE(dataUnchanged) | file.exists(here::here("app", "data", "forceUpdate.txt"))) &
    !is.null(countryData)

  if (condition) {
    cat(str_c("\n", Sys.time(), " | ", args["country"], ": New data available. Calculating Re ...\n"))

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
    names(constant_delay_symptom_to_report_distributions) <- paste0("Onset to ",  unique(names(shape_onset_to_count)))

    constant_delay_distributions <- c(constant_delay_distributions, constant_delay_symptom_to_report_distributions)

    # filter out regions with too few cases for estimation
    countryData <- countryData %>%
      filterRegions(thresholdConfirmedCases = 500)

    # filter out data_types with 0 total cases
    data_type0 <- countryData %>%
      group_by(data_type) %>%
      summarize(total = sum(value), .groups = "drop") %>%
      filter(total == 0) %>%
      .$data_type

    countryData <- filter(countryData, !(data_type %in% data_type0))

    # Filter for BAG
    countryData <- countryData %>%
      # Only Confirmed cases
      filter(data_type == "Confirmed cases") %>%
      # only country and cantons
      filter(!str_detect(region, "grR|seR"))

    if (nrow(countryData) > 0) {

      countryData <- countryData %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      right_truncation <- list()
      if (args["country"] %in% c("CHE", "LIE", "DEU", "HKG")) {
        right_truncation[["Confirmed cases"]] <- 0
      } else {
        right_truncation["Confirmed cases"] <- 3
      }

      right_truncate <- function(df, data_type, right_truncation) {
          dplyr::filter(df, date <= (max(date) - right_truncation[[unique(data_type)]]))
      }

      countryData <- countryData %>%
        group_by(country, region, source, data_type) %>%
        right_truncate(data_type, right_truncation) %>%
        dplyr::select(-countryIso3) %>%
        ungroup()

      if (file.exists(file.path(basePath, str_c(args["country"], "-Estimates.rds"))) &
          args["country"] == "CHE") {
        cat("Only updating recent estimates ...\n")
        cat("Complete Data Range:\n")
        print(range(countryData$date))
        # only calculate starting from dateCutoffAdj
        dateCutoff <- "2021-04-01"
        dateCutoffAdj <- "2021-03-01"

        countryData <- filter(countryData, date >= dateCutoffAdj)
        cat("Truncated data range:\n")
        print(range(countryData$date))
      } else {
        cat("New estimates. Calculating whole data series...\n")
      }
      # Deconvolution
      deconvolvedData <- list()

      deconvolvedCountryData <- get_all_infection_incidence(
        countryData,
        constant_delay_distributions = constant_delay_distributions,
        onset_to_count_empirical_delays = delays_onset_to_count,
        data_types = c("Confirmed cases"),
        n_bootstrap = 100,
        verbose = FALSE)

      countryDataPath <- file.path(basePath, str_c(args["country"], "-DeconvolutedData.rds"))
      if (dim(deconvolvedCountryData)[1] == 0) {
        print("no data remaining")
      } else {
        saveRDS(deconvolvedCountryData, file = countryDataPath)
        # Re Estimation
        cleanEnv(keepObjects = c("basePath", "tempPath", "deconvolvedCountryData", "args", "popData", "dateCutoff"))
        source(here::here("app/otherScripts/3_utils_doReEstimates.R"))

        swissRegions <- deconvolvedCountryData %>%
          filter(country %in% c("Switzerland", "Liechtenstein")) %>%
          dplyr::select(region) %>%
          distinct() %>%
          .$region

        ### Window
        window <- 3

        ### Delays applied
        all_delays <- list(
          "infection_Confirmed cases" = c(Cori = 0, WallingaTeunis = -5),
          "Confirmed cases" = c(Cori = 10, WallingaTeunis = 5))

        truncations <- list(
          left = c(Cori = 5, WallingaTeunis = 0),
          right = c(Cori = 0, WallingaTeunis = 8))

        ### Run EpiEstim
        countryEstimatesRaw <- doAllReEstimations(
          deconvolvedCountryData,
          slidingWindow = window,
          methods = "Cori",
          variationTypes = c("slidingWindow"),
          all_delays = all_delays,
          truncations = truncations,
          swissRegions = swissRegions)

        gc()
        cat("raw estimates done for ", args["country"], "\n")
        qs::qsave(countryEstimatesRaw, file = here::here(tempPath, "countryEstimatesRaw.qs"))
        rm(deconvolvedCountryData)
        gc()

        countryEstimates <- cleanCountryReEstimate(countryEstimatesRaw, method = "bootstrap") %>%
          left_join(
            dplyr::select(popData, region, countryIso3),
            by = c("region")
          )

        # add extra truncation of 4 days for all Swiss cantonal estimates due to consolidation
        if (args["country"] %in% c("CHE")) {
          days_truncated <- 4
          canton_list <- c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "JU", "LU", "NE", "SG", "SO", "SZ", "TG", "TI",
            "VD", "VS", "ZG", "ZH", "SH", "AR", "GL", "NW", "OW", "UR", "AI")

          countryEstimates_cantons <- countryEstimates %>%
            filter(region %in% canton_list) %>%
            group_by(country, region, source, data_type, estimate_type) %>%
              filter(row_number() <= (n() - days_truncated)) %>%
            ungroup()

          countryEstimates_CH <- countryEstimates %>%
            filter(!(region %in% canton_list))

          countryEstimates <- bind_rows(countryEstimates_cantons, countryEstimates_CH)

        }

        countryDataPath <- file.path(basePath, str_c(args["country"], "-Estimates.rds"))

        if (file.exists(file.path(countryDataPath)) & args["country"] == "CHE") {
          previousEstimates <- readRDS(countryDataPath) %>%
            filter(date < dateCutoff)
          newEstimates <- countryEstimates %>%
            filter(date >= dateCutoff)

          countryEstimates <- bind_rows(
            previousEstimates,
            newEstimates
          )
        }

        saveRDS(countryEstimates, file = countryDataPath)
        # Save as .csv for data upload
        readr::write_csv(countryEstimates,
                  path = file.path(basePath, "csv", str_c(args["country"], "-estimates.csv"))
        )
        # save simpler csvs for CHE, LIE
        if (args["country"] %in% c("CHE", "LIE")) {
          simpleCsv <- countryEstimates %>%
            filter(data_type == "Confirmed cases", estimate_type == "Cori_slidingWindow") %>%
            select(region, date, median_R_mean, median_R_highHPD, median_R_lowHPD) %>%
            mutate(across(.cols = median_R_mean:median_R_lowHPD, .fns = round, digits = 2))

          # write to csv directory
          readr::write_csv(
            simpleCsv,
            file = file.path(basePath, "csv", str_c(args["country"], "-confCasesSWestimates.csv"))
          )
        }
      }
    } else {
      cat(str_c(Sys.time(), " | ", args["country"], ": Not enough cases. Skipping Re calculation.\n"))
    }
  } else {
    cat(str_c(Sys.time(), " | ", args["country"], ": No new data available. Skipping Re calculation.\n"))
  }
} else {
  cat(str_c(Sys.time(), " | ", args["country"], ": No data available.\n"))
}
