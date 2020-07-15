# only show package messages when in interactive mode to clean up error logs
if (interactive()) {
  library("lubridate")
  library("fitdistrplus")
  library("EpiEstim")
  library("cbsodataR")
  library("here")
  library("tidyverse")
} else {
  suppressPackageStartupMessages({
    library("lubridate")
    library("fitdistrplus")
    library("EpiEstim")
    library("cbsodataR")
    library("here")
    library("tidyverse")
  })
}

source(here("app/otherScripts/1_utils_getRawData.R"))
source(here("app/otherScripts/utils.R"))

args <- commandArgs(trailingOnly = TRUE)
# testing
if (length(args) == 0) {
  args <- c("CHE")
  warning(str_c("Testing mode!! Country: ", args))
}
names(args) <- "country"

# Fetch Population Data (do once)
  popDataPath <- here("app", "data", "popData.rds")
  if (!file.exists(popDataPath)) {
    popDataWorldBank <- getCountryPopData() %>%
      filter(!(countryIso3 %in% c("LIE", "CHE"))) %>%
      mutate(region = countryIso3)
    popDataCH <- read_csv(
      file = here("app/data/additionalPopSizes.csv"),
      col_types = cols(
        .default = col_character(),
        populationSize = col_double()
      )
    )
    continents <- read_csv("app/data/continents.csv", col_types = cols(.default = col_character()))
    popData <- bind_rows(popDataWorldBank, popDataCH) %>%
      select(countryIso3, country, region, populationSize) %>%
      left_join(continents, by = "countryIso3")
    saveRDS(popData, file = popDataPath)
  } else {
    popData <- readRDS(popDataPath)
  }

# Fetch Country Data
  countryData <- getCountryData(
    args["country"],
    ECDCtemp = here("app/data/temp/ECDCdata.csv"),
    HMDtemp = here("app/data/temp/HMDdata.csv"),
    tReload = 30) %>%
    left_join(
      popData,
      by = c("countryIso3", "region")
    )

  # check for changes in country data
  countryDataPath <- here("app", "data", "countryData", str_c(args["country"], "-Data.rds"))
  if (file.exists(countryDataPath)) {
    countryDataOld <- readRDS(countryDataPath)
    dataUnchanged <- all.equal(countryData, countryDataOld)
  } else {
    dataUnchanged <- FALSE
  }

# save updated data
updateDataPath <- here("app", "data", "updateData.rds")
if (file.exists(updateDataPath)) {
  updateData <- readRDS(updateDataPath)
} else {
  updateData <- list()
}

if (!isTRUE(dataUnchanged)) {
  saveRDS(countryData, file = countryDataPath)
}

# fix because swiss data contains data for two countries (CHE & LIE)
for (i in unique(countryData$countryIso3)) {
  updateData[[i]] <- countryData %>%
    filter(countryIso3 == i) %>%
    mutate(
      data_type = replace(
        data_type,
        data_type %in% c("Hospitalized patients - onset", "Hospitalized patients - admission"),
        "Hospitalized patients")) %>%
    group_by(countryIso3, country, region, source, data_type) %>%
    summarize(lastData = max(date), .groups = "keep") %>%
    mutate(
      lastChanged = file.mtime(countryDataPath),
      lastChecked = Sys.time())
}

saveRDS(updateData, updateDataPath)

# get number of test data
if (args["country"] %in% c("CHE")) {
  testsDataPath <- here("app", "data", "countryData", str_c(args["country"], "-Tests.rds"))

  bagFiles <- list.files(here("app", "data", "BAG"),
    pattern = "*Time_series_tests.csv",
    full.names = TRUE,
    recursive = TRUE)

  bagFileDates <- strptime(
    stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
    format = "%Y-%m-%d_%H-%M-%S")

  newestFile <- bagFiles[which(bagFileDates == max(bagFileDates))[1]]
  nTests <- read_delim(file = newestFile, delim = ";",
    col_types = cols_only(
      Datum = col_date(format = ""),
      `Positive Tests` = col_double(),
      `Negative Tests` = col_double()
    )) %>%
    transmute(
      date = Datum,
      countryIso3 = "CHE",
      region = countryIso3,
      positiveTests = `Positive Tests`,
      negativeTests = `Negative Tests`,
      totalTests = positiveTests + negativeTests)

  saveRDS(nTests, testsDataPath)
}

cleanEnv(keepObjects = c("countryData", "dataUnchanged", "args", "popData"))

# calculate Re
# only if data has changed
if (!isTRUE(dataUnchanged)) {
  cat(str_c("\n", args["country"], ": New data available. Calculating Re ...\n"))
  # get Infection Incidence
    # load functions
      source(here("app/otherScripts/2_utils_getInfectionIncidence.R"))
    # load parameter
      source(here("app/otherScripts/2_params_InfectionIncidencePars.R"))
    # load empirical delays
      delays_data_path <- here("app/data/CH/FOPH_data_delays.csv")
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
    # filter out regions with to few cases for estimation
      countryData <- countryData %>%
        filterRegions(threshholdConfirmedCases = 500)
    # country specific data filtering
      if (args["country"] == "ESP") {
        countryData <- countryData %>%
          filter(data_type != "Deaths")
        cat("ignoring data_type Deaths\n")
      } else if (args["country"] == "AUT") {
        countryData <- countryData %>%
          filter(data_type != "Deaths")
        cat("ignoring data_type Deaths\n")
      } else if (args["country"] == "FRA") {
        countryData <- countryData %>%
          filter(data_type != "Hospitalized patients")
        cat("ignoring data_type Hospitalized patients\n")
      } else if (args["country"] == "CHE") {
        # no estimation for deaths per canton (too few cases)
        countryData <- countryData %>%
          filter(!(region != "CHE" & data_type == "Deaths"))
        cat("ignoring data_type Deaths on regional level\n")
      }
      countryData <- countryData %>%
        mutate(
          data_type = fct_drop(data_type)
        )

    # truncation
      right_truncation <- 2
      countryData <- countryData %>%
        group_by(country, region, source, data_type) %>%
        filter(date <= (max(date) - right_truncation)) %>%
        select(-countryIso3, -populationSize) %>%
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
        shape_incubation = shape_incubation,
        scale_incubation = scale_incubation,
        min_chi_squared = 1,
        maximum_iterations = 100,
        n_bootstrap = 50,
        verbose = FALSE)

      if ("Hospitalized patients - admission" %in% countryData$data_type |
          "Hospitalized patients - onset" %in% countryData$data_type) {
        deconvolvedData[[2]] <- get_all_infection_incidence(
          countryData,
          constant_delay_distributions = constant_delay_distributions,
          onset_to_count_empirical_delays = delays_onset_to_count,
          data_types = c("Hospitalized patients - admission",
                        "Hospitalized patients - onset"),
          shape_incubation = shape_incubation,
          scale_incubation = scale_incubation,
          min_chi_squared = 1,
          maximum_iterations = 100,
          n_bootstrap = 50,
          verbose = F)
        deconvolvedData[[2]] <- deconvolvedData[[2]] %>%
          group_by(date, country, region, data_type, source, replicate, variable) %>%
          summarise(value = sum(value), .groups = "keep") %>%
          arrange(country, region, source, data_type, variable, replicate, date) %>%
          ungroup()
      }
      deconvolvedCountryData <- bind_rows(deconvolvedData)
      countryDataPath <- here("app", "data", "countryData", str_c(args["country"], "-DeconvolutedData.rds"))
      saveRDS(deconvolvedCountryData, file = countryDataPath)

    # Re Estimation
      cleanEnv(keepObjects = c("deconvolvedCountryData", "args", "popData"))
      source(here("app/otherScripts/3_utils_doReEstimates.R"))
      pathToAdditionalData <- here("../covid19-additionalData/interventions/")

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

      ### Delays applied
      all_delays <- list(
        "infection_Confirmed cases" = c(Cori = 0, WallingaTeunis = -5),
        "infection_Deaths" = c(Cori = 0, WallingaTeunis = -5),
        "infection_Hospitalized patients" = c(Cori = 0, WallingaTeunis = -5),
        "Confirmed cases" = c(Cori = 10, WallingaTeunis = 5),
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
              "infection_Hospitalized patients",
              "infection_Deaths",
              "infection_Excess deaths"),
            labels = c(
              "Confirmed cases",
              "Hospitalized patients",
              "Deaths",
              "Excess deaths"))) %>%
        pivot_wider(names_from = "variable", values_from = "value") %>%
        group_by(date, country, region, data_type, source, estimate_type) %>%
        summarize(
          median_R_mean = median(R_mean),
          median_R_highHPD = median(R_highHPD),
          median_R_lowHPD = median(R_lowHPD),
          .groups = "keep"
        ) %>%
        select(country, region, source, data_type, estimate_type, date,
          median_R_mean, median_R_highHPD, median_R_lowHPD) %>%
        arrange(country, region, source, data_type, estimate_type, date) %>%
        ungroup() %>%
        left_join(
          select(popData, country, region, countryIso3),
          by = c("country", "region")
        )
      countryDataPath <- here("app", "data", "countryData", str_c(args["country"], "-Estimates.rds"))
      saveRDS(countryEstimates, file = countryDataPath)
} else {
  cat(str_c(args["country"], ": No new data available. Skipping Re calculation.\n"))
}
