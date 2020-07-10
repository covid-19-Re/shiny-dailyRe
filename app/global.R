library(tidyverse)
library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)

source("otherScripts/ReffPlotly.R")
source("../utils.R")

# source for Loess function
source("otherScripts/2_utils_getInfectionIncidence.R")

dataDir <- "data"

translator <- Translator$new(translation_json_path = file.path(dataDir, "shinyTranslations.json"))
languageSelect <- translator$languages
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToRawData <- file.path(dataDir, "Raw_data.rds")
pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.rds")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.rds")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.rds")
pathTolatestData <- file.path(dataDir, "latestData.rds")
pathToLastCheck <- file.path(dataDir, "lastCheck.txt")
pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToCountryList <- file.path(dataDir, "countryList.rds")
pathToEstimatesDates <- file.path(dataDir, "estimate_dates.rds")
pathToValidEstimates <- file.path(dataDir, "valid_estimates.rds")
infection_data_file_path <- file.path(dataDir,  "Deconvolved_infect_data.rds")

pathToPopSizes <- file.path(dataDir, "popSizes.Rdata")

countryList <- readRDS(pathToCountryList)
