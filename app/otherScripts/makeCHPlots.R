library(tidyverse)
library(plotly)
library(here)
library(shiny.i18n)

source(here::here("app", "otherScripts", "ReffPlotly.R"))
source(here::here("app", "utils.R"))

# load data
dataDir <- here("app/data/")
pathToCaseData <- file.path(dataDir, "SwitzerlandData.rds")
pathToEstimates <- file.path(dataDir, "SwitzerlandEstimates.rds")
pathToUpdateData <- file.path(dataDir, "updateData.rds")
pathToInterventionData <- here("../covid19-additionalData/interventions/interventions.csv")

caseData <- readRDS(pathToCaseData) %>%
  pivot_wider(names_from = "variable", values_from = "value")
estimates <- readRDS(pathToEstimates)
updateData <- readRDS(pathToUpdateData)[["Switzerland"]]

# prepare Data
caseDataPlot <- caseData %>%
  filter(
    country == "Switzerland",
    region == "Switzerland",
    source %in% c("FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(data_type = fct_drop(data_type))

estimatePlotRanges <- estimateRanges(caseDataPlot,
  minConfirmedCases = 100,
  delays = delaysDf)

estimatesPlot <- estimates %>%
  filter(
    estimate_type == "Cori_slidingWindow",
    country == "Switzerland",
    region == "Switzerland",
    source %in% c("FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    region = fct_drop(region),
    country = fct_drop(country),
    data_type = fct_drop(data_type)
  ) %>%
  group_by(data_type) %>%
  filter(
    between(date,
      left = earliestEstimate,
      right = max(date) - delays[[as.character(data_type[1])]])
  ) %>%
  ungroup()

updateData

latestDataPlot <- updateData %>%
  ungroup() %>%
  filter(
    country == "Switzerland",
    region == "Switzerland",
    source %in% unique(estimates$source)) %>%
  distinct()

translator <- Translator$new(translation_json_path = here::here("app", "data", "shinyTranslations.json"))

plotOutDir <- here::here("app/www")

interventions <- read_csv(file = pathToInterventionData,
  col_types = cols(
    name = col_character(),
    y = col_double(),
    text = col_character(),
    tooltip = col_character(),
    type = col_character(),
    date = col_date(format = ""),
    plotTextPosition = col_character())) %>%
  filter(country == "Switzerland")


for (i in translator$languages) {

  translator$set_translation_language(i)

  interventionsLocalized <- interventions %>%
      mutate(
        text = sapply(text, translator$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, translator$t,  USE.NAMES = FALSE))

  latestDataPlotLocalized <- latestDataPlot %>%
    mutate(
      source = sapply(source, translator$t,  USE.NAMES = FALSE),
      data_type = sapply(as.character(data_type), translator$t,  USE.NAMES = FALSE)
    )

  plotlyPlotV <- rEffPlotly(
    caseDataPlot,
    estimatesPlot,
    interventionsLocalized,
    plotColors,
    lastDataDate = latestDataPlotLocalized,
    fixedRangeX = c(TRUE, TRUE, TRUE),
    fixedRangeY = c(TRUE, TRUE, TRUE),
    language = i,
    translator = translator,
    widgetID = "rEffplots")

  plotlyPlotV$sizingPolicy$browser$padding <- 0

  plotlyPlotV

  htmlwidgets::saveWidget(plotlyPlotV,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")

  # write_lines(
  #   htmlwidgetsExtended::exportWidgetJson(plotlyPlotV),
  #   file.path(plotOutDir, str_c("rEffplotly_data_", i, ".json")))
}

writeLines(str_c("last check: ", Sys.time()), file.path(dataDir, "lastCheck.txt"))

cat(paste("###", Sys.time(), "- done 5_makeReffPlotly.R", "\n"))

startTime <- readRDS(file = "ScriptStartTime.rds")

cat(paste("###", "total script duration:", signif(difftime(Sys.time(), startTime, units = "mins")), "min"))
