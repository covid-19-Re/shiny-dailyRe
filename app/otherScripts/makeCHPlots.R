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

pathToCountryData <- here::here("app", "data", "countryData")
pathToUpdateData <- file.path(dataDir, "updateData.rds")
pathToInterventionData <- here::here("../covid19-additionalData/interventions/interventions.csv")
pathToContinentsData <- file.path(dataDir, "continents.csv")
continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

countryData <- loadCountryData(countrySelectValue, dataDir = pathToCountryData)

countryData$estimates <- countryData$estimates %>%
  filter(data_type != "Stringency Index") %>%
  group_by(countryIso3, data_type) %>%
  filter(
      between(date,
        left = countryData$estimateRanges[[countrySelectValue]][[countrySelectValue]][["start"]][[as.character(data_type[1])]],
        right = countryData$estimateRanges[[countrySelectValue]][[countrySelectValue]][["end"]][[as.character(data_type[1])]])
    ) %>%
  mutate(data_type = as.character(data_type))


updateDataRaw <- readRDS(pathToUpdateData)
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

translator <- Translator$new(translation_json_path = file.path(dataDir, "shinyTranslations.json"))
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
      lang = i,
      plotSize = "large"),
    rightTruncation,
    translator,
    showHelpBox = TRUE)

  plot$sizingPolicy$browser$padding <- 0

  saveWidget(plot,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")
}

cat("done making CH plots for ncs-tf website.\n")
