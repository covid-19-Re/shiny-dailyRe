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

# enableBookmarking(store = "url")

source("otherScripts/plotlyFunctions.R")
source("utils.R")

# set up translation
translator <- Translator$new(translation_json_path = file.path("data/shinyTranslations.json"))
languageSelect <- translator$get_languages()
names(languageSelect) <- c("EN", "IT", "FR", "DE")

# load data
allData <- qs::qread("data/serialized/allCountryData.qs")

continents <- read_csv("data/continents.csv", col_types = cols(.default = col_character()))
countryList <- qs::qread("data/serialized/countryList.qs")

interventionsData <- qs::qread("data/serialized/interventionsData.qs")

updateDataRaw <-  qs::qread("data/serialized/updateDataRaw.qs")
dataSources <-  qs::qread("data/serialized/dataSources.qs")

worldMapData <-  qs::qread("data/serialized/worldMapData.qs")
countriesShape <-  qs::qread("data/serialized/countriesShape.qs")
CHEregionsShape <-  qs::qread("data/serialized/CHEregionsShape.qs")
cheCasesLabels <-  qs::qread("data/serialized/cheCasesLabels.qs")
cheReLabels <-  qs::qread("data/serialized/cheReLabels.qs")

ZAFregionsShape <-  qs::qread("data/serialized/ZAFregionsShape.qs")
zafCasesLabels <-  qs::qread("data/serialized/zafCasesLabels.qs")
zafReLabels <-  qs::qread("data/serialized/zafReLabels.qs")

CHErightTruncation <- qs::qread("data/countryData/CHE-addRightTruncation.qs")
