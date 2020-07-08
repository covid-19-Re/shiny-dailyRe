library("lubridate")
library("fitdistrplus")
library("cbsodataR")
library("tidyverse")
library("here")

source(here("app/otherScripts/1_utils_getRawData.R"))
source(here("app/otherScripts/countryScripts/utils.R"))

args <- commandArgs(trailingOnly = TRUE)
# testing
if (length(args) == 0) {
  warning("Testing mode!!")
  args <- c("Switzerland")
}
names(args) <- "country"

# Fetch Data
  countryData <- getCountryData(args["country"])
  
  # check for changes in country data
  countryDataPath <- here("app", "data", "countries", str_c(args["country"], "Data.rds"))
  if (file.exists(countryDataPath)) {
    countryDataOld <- readRDS(countryDataPath)
    dataUnchanged <- all.equal(countryData, countryDataOld)
  } else {
    countryDataOld <- tibble()
    dataUnchanged <- FALSE
  }

  

# calculate Re
# only if data has changed
if (!isTRUE(dataUnchanged)) {
  saveRDS(countryData, file = countryDataPath)
  cleanEnv(keepObjects = c("countryData", "dataUnchanged", "args"))
  cat(args["country"], ": New data available. Calculating Re ...")
  
  # get Infection Incidence
    # load functions
      source("app/otherScripts/2_utils_getInfectionIncidence.R")
    # load parameter
      source("app/otherScripts/countryScripts/utils_InfectionIncidencePars.R")
    # load empirical delays
      delays_data_path <- here("app/data/CH/FOPH_data_delays.csv")
      delays_onset_to_count <- read_csv(delays_data_path,
        col_types = cols(
          data_type = col_character(),
          onset_date = col_date(format = ""),
          count_date = col_date(format = ""),
          delay = col_number()))
    # filter out regions with to few cases for estimation
      countryData <- countryData %>%
        filterRegions(threshholdConfirmedCases = 500)
    # country specific data filtering
      if (args["country"] == "Spain") {
        countryData <- countryData %>%
          filter(data_type != "Deaths")
      } else if (args["country"] == "Austria") {
        countryData <- countryData %>%
          filter(data_type != "Deaths")
      } else if (args["country"] == "France") {
        countryData <- countryData %>%
          filter(data_type != "Hospitalized patients")
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
        ungroup()
    # Deconvolution
      deconvolvedData <- list()

      deconvolvedData[[1]] <- get_all_infection_incidence(
        countryData,
        onset_to_count_empirical_delays = delays_onset_to_count,
        data_types = c(
          "Confirmed cases",
          "Hospitalized patients",
          "Deaths"),
        shape_incubation = shape_incubation,
        scale_incubation = scale_incubation,
        shape_onset_to_count = shape_onset_to_count,
        scale_onset_to_count = scale_onset_to_count,
        min_chi_squared = 1,
        maximum_iterations = 20,
        n_bootstrap = 50,
        verbose = FALSE)

      if ("Hospitalized patients - admission" %in% countryData$data_type |
          "Hospitalized patients - onset" %in% countryData$data_type) {
        deconvolvedData[[2]] <- get_all_infection_incidence(
          countryData,
          onset_to_count_empirical_delays = delays_onset_to_count,
          data_types = c(
            "Hospitalized patients - admission",
            "Hospitalized patients - onset"),
          shape_incubation = shape_incubation,
          scale_incubation = scale_incubation,
          shape_onset_to_count = shape_onset_to_count,
          scale_onset_to_count = scale_onset_to_count,
          min_chi_squared = 1,
          maximum_iterations = 20,
          n_bootstrap = 50,
          verbose = FALSE)
        deconvolvedData[[2]] <- deconvolvedData[[2]] %>%
          group_by(date, country, region, data_type, source, replicate, variable) %>%
          summarise(value = sum(value)) %>%
          arrange(country, region, source, data_type, variable, replicate, date) %>%
          ungroup()
      }
      deconvolvedData <- bind_rows(deconvolvedData) %>%
        ungroup()

} else {
  cat(args["country"], ": No new data available. Skipping Re calculation.")
}
