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

basePath <- here::here("app", "data", "countryData")
if (!dir.exists(basePath)) {
  dir.create(basePath)
}

# fetch stringency data
if (args["country"] == "CHE") {
  stringencyData <- read_csv(
    "https://raw.githubusercontent.com/KOF-ch/economic-monitoring/master/data/ch.kof.stringency.csv",
    col_types = cols(
      time = col_date(format = ""),
      geo = col_character(),
      variable = col_character(),
      value = col_double()
    )) %>%
    filter(
      variable == "stringency") %>%
    dplyr::transmute(
      date = time,
      countryIso3 = "CHE",
      region = recode(toupper(geo), "CH" = "CHE"),
      source = "KOF",
      StringencyIndex = value
    )
} else {
  stringencyData <- getDataOxfordStringency(countries = args["country"],
    tempFileName = here::here("app/data/temp/oxfordStringency.csv"), tReload = 300) %>%
    mutate(source = "BSG Covidtracker")
}

stringencyDataPath <- file.path(basePath, str_c(args["country"], "-OxCGRT.rds"))

if (file.exists(stringencyDataPath)) {
  stringencyDataOld <- readRDS(stringencyDataPath)
  # if new data is null, keep old data (can happen because of error in reading new data)
  if (is.null(stringencyData)) {
    stringencyData <- stringencyDataOld
  }
  stringencyUnchanged <- all.equal(stringencyData, stringencyDataOld)
} else {
  stringencyUnchanged <- FALSE
}

if (!isTRUE(stringencyUnchanged)) {
  saveRDS(stringencyData, file = stringencyDataPath)
}

stringencyIndex <- stringencyData %>%
  dplyr::transmute(
    date,
    countryIso3,
    region,
    data_type = "Stringency Index",
    source = source,
    value = StringencyIndex
  ) %>%
  filter(!is.na(value)) %>%
  arrange(countryIso3, region, date)

# in Re estimation, the interval starts on interval_end + 1
# so the intervention start dates need to be shifted to - 1
interval_ends_df <- stringencyIndex %>%
  filter(c(0, diff(stringencyIndex$value, 1, 1)) != 0) %>%
  mutate(interval_ends = date - 1) %>%
  dplyr::select(region, interval_ends)

interval_ends <- split(interval_ends_df$interval_ends, interval_ends_df$region)
interval_ends[["default"]] <- interval_ends[[args["country"]]]
# Fetch Country Data
countryData <- getCountryData(
  args["country"],
  tempFile = here::here("app/data/temp/ECDCdata.csv"),
  HMDtemp = here::here("app/data/temp/HMDdata.csv"),
  tReload = 300) %>%
  left_join(
    popData,
    by = c("countryIso3", "region")
  ) %>%
  bind_rows(
    mutate(stringencyIndex,
      date_type = if_else(
        args["country"] %in% c("CHE", "DEU", "HKG"), "report_plotting", "report"))
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
    # save updated data
    if (!isTRUE(dataUnchanged)) {
      saveRDS(countryData, file = countryDataPath)
    }
  }

  cleanEnv(keepObjects = c("basePath", "countryData", "dataUnchanged", "args", "popData", "interval_ends"))

  # calculate Re
  # only if (data has changed OR forceUpdate.txt exists) AND countryData is not null
  condition <- (!isTRUE(dataUnchanged) | file.exists(here::here("app", "data", "forceUpdate.txt"))) &
    !is.null(countryData)

  if (condition) {
    cat(str_c("\n", Sys.time(), " | ", args["country"], ": New data available. Calculating Re ...\n"))
    # send notification
    if (args["country"] %in% c("CHE") &
        file.exists(here::here("app/otherScripts/sendNotifications.txt"))) {
      urls <- scan(here::here("app/otherScripts/slackWebhook.txt"), what = "character")

      sendSlackNotification(
        country = args["country"],
        event = "newData", url = urls[1], eTcompletion = Sys.time() + 120 * 60, webhookUrl = urls[2])
    }
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
    }

    if (nrow(countryData) > 0) {

      countryData <- countryData %>%
        mutate(
          data_type = fct_drop(data_type)
        )

      right_truncation <- list()
      if (args["country"] %in% c("CHE", "LIE", "DEU", "HKG")) {
        right_truncation[["Confirmed cases"]] <- 0
        right_truncation[["Confirmed cases / tests"]] <- 0
        right_truncation[["Hospitalized patients"]] <- 0
        right_truncation[["Deaths"]] <- 0
      } else {
        right_truncation["Confirmed cases"] <- 3
        right_truncation["Confirmed cases / tests"] <- 3
        right_truncation["Hospitalized patients"] <- 3
        right_truncation["Deaths"] <- 3
      }

      right_truncate <- function(df, data_type, right_truncation) {
          dplyr::filter(df, date <= (max(date) - right_truncation[[unique(data_type)]]))
      }

      countryData <- countryData %>%
        group_by(country, region, source, data_type) %>%
        right_truncate(data_type, right_truncation) %>%
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
        n_bootstrap = 100,
        verbose = FALSE)

      if (args["country"] %in% c("CHE")) {
        countryDataTests <- countryData %>%
          filter(data_type == "Confirmed cases / tests")

        deconvolvedData[[2]] <- get_all_infection_incidence(
          countryDataTests,
          constant_delay_distributions = constant_delay_distributions,
          onset_to_count_empirical_delays = delays_onset_to_count,
          data_types = c("Confirmed cases / tests"),
          n_bootstrap = 100,
          verbose = FALSE)
      }

      deconvolvedCountryData <- bind_rows(deconvolvedData)
      countryDataPath <- file.path(basePath, str_c(args["country"], "-DeconvolutedData.rds"))
      if (dim(deconvolvedCountryData)[1] == 0) {
        print("no data remaining")
      } else {
        saveRDS(deconvolvedCountryData, file = countryDataPath)
        # Re Estimation
        cleanEnv(keepObjects = c("basePath", "deconvolvedCountryData", "args", "popData", "interval_ends"))
        source(here::here("app/otherScripts/3_utils_doReEstimates.R"))

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
          variationTypes = c("step", "slidingWindow"),
          #variationTypes = c("slidingWindow"),
          all_delays = all_delays,
          truncations = truncations,
          interval_ends = interval_ends,
          swissRegions = swissRegions)

        
        countryEstimates <- cleanCountryReEstimate(countryEstimatesRaw, method = 'bootstrap') %>%
          left_join(
            dplyr::select(popData, region, countryIso3),
            by = c("region")
          )

        # add extra truncation of 4 days for all Swiss cantonal estimates due to consolidation
        if (args["country"] %in% c("CHE")) {
          days_truncated <- 4
          canton_list <- c("AG", "BE", "BL","BS", "FR", "GE", "GR", "JU", "LU", "NE", "SG", "SO", "SZ", "TG", "TI",
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
          if (Sys.info()["nodename"] == "ibz-shiny.ethz.ch") {
            # write to test directory
            readr::write_csv(
              simpleCsv,
              file = str_c("/home/covid-19-re/test-dailyRe/app/www/", args["country"], "-confCasesSWestimates.csv")
            )
            if (str_detect(here(), "test")) {
              # write estimates to main app for publication
              saveRDS(countryEstimates,
                file = str_c("/home/covid-19-re/dailyRe/app/data/countryData/",
                  args["country"], "-estimates.rds")
              )
            }
          }
        }
      }
    } else {
      cat(str_c(Sys.time(), " | ", args["country"], ": Not enough cases. Skipping Re calculation.\n"))
    }
    # send notification
    if (args["country"] %in% c("CHE") &
        file.exists(here::here("app/otherScripts/sendNotifications.txt"))) {
      write(args["country"], file = here::here("app/otherScripts/notificationsToSend.txt"), append = TRUE)
    }
  } else {
    cat(str_c(Sys.time(), " | ", args["country"], ": No new data available. Skipping Re calculation.\n"))
  }
} else {
  cat(str_c(Sys.time(), " | ", args["country"], ": No data available.\n"))
}
