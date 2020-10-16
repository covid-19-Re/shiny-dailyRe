library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shinycssloaders)
library(shinyBS)

source("otherScripts/plotlyFunctions.R")
source("utils.R")

# set up translation
translator <- Translator$new(translation_json_path = file.path("data/shinyTranslations.json"))
languageSelect <- translator$get_languages()
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToPopData <- file.path("data/popData.rds")
pathToUpdataData <- file.path("data/updateData.rds")
pathToCountryData <- "data/countryData"
pathToContinentsData <- file.path("data/continents.csv")
pathToAllCountryData <- file.path("data/allCountryData.rds")


popData <- readRDS(pathToPopData) %>% filter(!is.na(countryIso3))
continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

countryList <- tibble(
  countryIso3 = unique(
    str_match(
      string = list.files(path = pathToCountryData, pattern = ".*-Estimates", recursive = TRUE),
      pattern = "(.*)-.*"
    )[, 2])) %>%
  left_join(
    continents,
    by = "countryIso3") %>%
  arrange(continent, country) %>%
  split(f = .$continent) %>%
  lapply(function(df) {
    df <- df %>% distinct()
    selectList <- df$countryIso3
    names(selectList) <- df$country
    return(selectList)
  })
