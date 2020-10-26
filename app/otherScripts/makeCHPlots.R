if (interactive()) {
  library(plotly)
  library(here)
  library(shiny.i18n)
  library(htmlwidgets)
  library(tidyverse)
} else {
  suppressPackageStartupMessages({
    library(plotly)
    library(here)
    library(shiny.i18n)
    library(htmlwidgets)
    library(tidyverse)
  })
}

source(here::here("app", "otherScripts", "plotlyFunctions.R"))
source(here::here("app", "utils.R"))

countrySelectValue <- "CHE"
cat(str_c("making ", countrySelectValue, " plots for ncs-tf website ...\n"))

# load data
dataDir <- here::here("app/data")
plotOutDir <- here::here("app/www")

pathToAllCountryData <- file.path(dataDir, "allCountryData.rds")
pathToUpdateData <- file.path(dataDir, "updateData.rds")
pathToInterventionData <- here::here("../covid19-additionalData/interventions/interventions.csv")
pathToContinentsData <- file.path(dataDir, "continents.csv")

continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))
allData <- readRDS(pathToAllCountryData)
countryData <- list(
  caseData = filter(allData$caseData, countryIso3 %in% countrySelectValue),
  estimates = filter(allData$estimates, countryIso3 %in% countrySelectValue)
)

updateDataRaw <- readRDS(pathToUpdateData)
updateData <- bind_rows(updateDataRaw[countrySelectValue]) %>%
  ungroup() %>%
  select(-country) %>%
  left_join(select(continents, countryIso3, country), by = "countryIso3")

interventions <- read_csv(
  str_c(pathToInterventionData),
  col_types = cols(
    .default = col_character(),
    date = col_date(format = ""),
    y = col_double()
  )) %>%
  split(f = .$countryIso3)

translator <- Translator$new(translation_json_path = file.path(dataDir, "shinyTranslations.json"))
availableLanguages <- translator$get_languages()


for (i in availableLanguages) {

  translator$set_translation_language(i)

  plot <- rEffPlotlyShiny(
    countryData,
    updateData,
    interventions,
    seriesSelect = "data_type",
    input = list(
      estimationTypeSelect = "Cori_slidingWindow",
      plotOptions = c("none"),
      caseAverage = 1,
      lang = i,
      plotSize = "large"),
    translator,
    showHelpBox = TRUE)

  plot$sizingPolicy$browser$padding <- 0

  saveWidget(plot,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")
}

cat("done making CH plots for ncs-tf website.\n")
