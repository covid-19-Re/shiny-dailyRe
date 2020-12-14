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

pathToCountryData <- file.path(dataDir, "serialized", "allCountryData.qs")
pathToUpdateData <- file.path(dataDir, "serialized", "updateDataRaw.qs")
pathToInterventionData <- here::here("../covid19-additionalData/interventions/interventions.csv")
pathToContinentsData <- file.path(dataDir, "continents.csv")
continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

allData <- qs::qread(pathToCountryData)
countryData <- list(
  caseData = filter(allData$caseData, countryIso3 %in% countrySelectValue),
  estimates = filter(allData$estimates, countryIso3 %in% countrySelectValue)
)

updateDataRaw <-  qs::qread(pathToUpdateData)
updateData <- bind_rows(updateDataRaw[countrySelectValue]) %>%
  ungroup() %>%
  dplyr::select(-country) %>%
  left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3")

interventions <- read_csv(
  str_c(pathToInterventionData),
  col_types = cols(
    .default = col_character(),
    date = col_date(format = ""),
    y = col_double()
  )) %>%
  split(f = .$countryIso3)

translator <- Translator$new(translation_json_path = file.path(dataDir, "covid19reTranslations.json"))
availableLanguages <- translator$get_languages()

rightTruncation <- list(
  "CHE" = list(
    "Confirmed cases" = 3,
    "Confirmed cases / tests" = 3,
    "Hospitalized patients" = 5,
    "Deaths" = 5)
)

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
      lang = i),
    rightTruncation,
    translator,
    plotSize = "small",
    showHelpBox = FALSE)

  plot$sizingPolicy$browser$padding <- 0

  saveWidget(plot,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")
}

cat("done making CH plots for ncs-tf website.\n")
