library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)
library(tidyverse)

source("otherScripts/ReffPlotly.R")
source("utils.R")

# set up translation
translator <- Translator$new(translation_json_path = file.path("data/shinyTranslations.json"))
languageSelect <- translator$languages
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToPopData <- file.path("data/popData.Rds")
pathToUpdataData <- file.path("data/updateData.Rds")
pathToCountryData <- "data/countryData"
