library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)
library(tidyverse)
library(leaflet)
library(htmltools)
library(htmlwidgets)

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
pathToContinentsData <- file.path("data/continents.csv")
pathToAllCountryData <- file.path("data/allCountryData.rds")


popData <- readRDS(pathToPopData)
continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

countryList <- tibble(
  countryIso3 = unique(
    str_match(
      string = list.files(path = pathToCountryData, pattern = ".*-Estimates", recursive = TRUE),
      pattern = "(.*)-.*"
    )[, 2])) %>%
  left_join(select(popData, countryIso3), by = "countryIso3") %>%
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

# geodata

  countriesShape <- rgdal::readOGR(
    dsn = "data/geoData/",
    layer = "ne_50m_admin_0_countries",
    stringsAsFactors = FALSE)

  CHEregionsShape <- rgdal::readOGR(
    dsn = "data/geoData/",
    layer = "swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET",
    stringsAsFactors = FALSE) %>%
    sp::spTransform(countriesShape@proj4string)

  cantonNames <- tibble(
    region = c(
      "ZH", "BE", "LU", "UR", "SZ",
      "OW", "NW", "GL", "ZG", "FR",
      "SO", "BS", "BL", "SH", "AR",
      "AI", "SG", "GR", "AG", "TG",
      "TI", "VD", "VS", "NE", "GE", "JU"),
    KANTONSNUM = c(
      1:26
    )
  )

  CHEregionsShape@data$KANTONSNUM <- as.integer(CHEregionsShape@data$KANTONSNUM)

  CHEregionsShape@data <- left_join(
    CHEregionsShape@data,
    cantonNames,
    by = "KANTONSNUM"
  )
  CHEregionsShape@data$ADM0_A3_IS <- "CHE"

  ZAFregionsShape <- rgdal::readOGR(
    dsn = "data/geoData/",
    layer = "zaf_admbnda_adm1_2016SADB_OCHA",
    stringsAsFactors = FALSE)
  ZAFregionsShape@data$ADM0_A3_IS <- "ZAF"
  ZAFregionsShape@data$region <- ZAFregionsShape@data$ADM1_EN %>% recode("Nothern Cape" = "Northern Cape")
  ZAFregionsShape@data$NAME <- ZAFregionsShape@data$region
