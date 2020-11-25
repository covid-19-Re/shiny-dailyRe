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
pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToUpdataData <- file.path("data/updateData.rds")
pathToCountryData <- "data/countryData"
pathToContinentsData <- file.path("data/continents.csv")
pathToAllCountryData <- file.path("data/allCountryData.rds")

allData <- readRDS(pathToAllCountryData)

continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

countryList <- tibble(
    countryIso3 = unique(allData$estimates$countryIso3)
  ) %>%
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

interventionsData <- read_csv(
  str_c(pathToInterventionData, "interventions.csv"),
  col_types = cols(
    .default = col_character(),
    date = col_date(format = ""),
    y = col_double()
  )) %>%
  split(f = .$countryIso3)

updateDataRaw <- readRDS(pathToUpdataData)
sourceInfo <- read_csv("data/dataSources.csv", col_types = cols(.default = col_character()))
dataSources <- updateDataRaw %>%
  bind_rows() %>%
  ungroup() %>%
  dplyr::select(countryIso3, source, data_type, lastData) %>%
  filter(
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths", "Excess deaths", "Stringency Index")
  ) %>%
  left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3") %>%
  left_join(sourceInfo, by = "source") %>%
  group_by(source, sourceLong, url) %>%
  dplyr::summarize(
    countries = if_else(length(unique(country)) > 5, "other Countries", str_c(unique(country), collapse = ", ")),
    data_type = str_c(as.character(unique(data_type)), collapse = ", "),
    .groups = "drop_last") %>%
  mutate(url = if_else(url != "", str_c("<a href=", url, ">link</a>"), "")) %>%
  dplyr::select("Source" = source, "Description" = sourceLong,
    "Countries" = countries, "Data types" = data_type, "URL" = url)

estimates <- allData$estimates %>%
  filter(
    data_type == "Confirmed cases",
    estimate_type == "Cori_slidingWindow") %>%
  group_by(region) %>%
  filter(date == max(date)) %>%
  dplyr::select(
      ADM0_A3_IS = countryIso3,
      region = region,
      estimate_type,
      data_typeEstimate = data_type,
      dateEstimates = date,
      median_R_mean,
      median_R_highHPD,
      median_R_lowHPD)

worldMapData <-  allData$caseData %>%
  bind_rows() %>%
  ungroup() %>%
  filter(
    data_type == "Confirmed cases",
    #sanitize
    !is.na(date)) %>%
  arrange(countryIso3, region, data_type, date) %>%
  group_by(region) %>%
  mutate(
    sum14days = slide_index_dbl(value, date, sum, .before = lubridate::days(14))
  ) %>%
  ungroup() %>%
  mutate(cases14d = sum14days / populationSize * 100000) %>%
  dplyr::select(
    ADM0_A3_IS = countryIso3,
    region = region,
    sourceCases = source,
    data_typeCases = data_type,
    dateCases = date,
    nCases = value,
    cases14d,
    populationSize) %>%
  group_by(ADM0_A3_IS, region) %>%
  filter(dateCases == max(dateCases)) %>%
  left_join(estimates, by = c("ADM0_A3_IS", "region")) %>%
  ungroup() %>%
  distinct()

countriesShape <- left_join(
  st_read("data/geoData/ne_50m_admin_0_countries.shp", quiet = TRUE),
  filter(worldMapData, region == ADM0_A3_IS),
  by = "ADM0_A3_IS")

CHEregionsShape <- st_read("data/geoData/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp", quiet = TRUE) %>%
  st_transform(st_crs(countriesShape)) %>%
  st_zm() %>%
  left_join(
    tibble(
      region = c(
        "ZH", "BE", "LU", "UR", "SZ",
        "OW", "NW", "GL", "ZG", "FR",
        "SO", "BS", "BL", "SH", "AR",
        "AI", "SG", "GR", "AG", "TG",
        "TI", "VD", "VS", "NE", "GE", "JU"),
      KANTONSNUM = c(
        1:26
      )),
    by = "KANTONSNUM") %>%
  mutate(
    ADM0_A3_IS = "CHE"
  ) %>%
  left_join(
    worldMapData,
    by = c("ADM0_A3_IS", "region")
  )
cheCasesLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "cases14d")
cheReLabels <- mapLabels(shapeFileData = CHEregionsShape, mainLabel = "re")

ZAFregionsShape <- st_read("data/geoData/zaf_admbnda_adm1_2016SADB_OCHA.shp", quiet = TRUE) %>%
  mutate(
    ADM0_A3_IS = "ZAF",
    region = recode(ADM1_EN, "Nothern Cape" = "Northern Cape"),
    NAME = region
  ) %>%
  left_join(
    worldMapData,
    by = c("ADM0_A3_IS", "region"))
zafCasesLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "cases14d")
zafReLabels <- mapLabels(shapeFileData = ZAFregionsShape, mainLabel = "re")
