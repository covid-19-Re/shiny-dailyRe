library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)
library(tidyverse)

source("otherScripts/plotlyFunctions.R")
source("utils.R")

# set up translation
translator <- Translator$new(translation_json_path = file.path("data/shinyTranslations.json"))
languageSelect <- translator$languages
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToPopData <- file.path("data/popData.rds")
pathToUpdataData <- file.path("data/updateData.rds")
pathToCountryData <- "data/countryData"

popData <- readRDS(pathToPopData)

countryList <- tibble(
  countryIso3 = unique(
    str_match(
      string = list.files(path = pathToCountryData, pattern = ".*-Estimates", recursive = TRUE),
      pattern = "/(.*)-.*"
    )[, 2])) %>%
  left_join(
    distinct(dplyr::select(popData, countryIso3, country, continent)),
    by = "countryIso3") %>%
  arrange(continent, country) %>%
  split(f = .$continent) %>%
  lapply(function(df) {
    selectList <- df$countryIso3
    names(selectList) <- df$country
    return(selectList)
  })
