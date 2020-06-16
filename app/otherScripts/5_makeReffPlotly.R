cat(paste("###", Sys.time(), "- starting 5_makeReffPlotly.R", "\n"))

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(here)
library(viridisLite)
library(shiny.i18n)

# load data
dataDir <- here::here("app/data/temp")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")
pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToEstimateDates <- file.path(dataDir, "estimate_dates.Rdata")
pathToInterventionData <- here::here("../covid19-additionalData/interventions/interventions.csv")

load(pathToRawData)
load(pathToEstimatesReSum)
load(pathToLatestData)
load(pathToEstimateDates)

# common functions
dataUpdatesString <- function(latestData, name = "Data Source", dateFormat = "%Y-%m-%d") {
  outList <- list(str_c(name, ": "))
  for (i in 1:dim(latestData)[1]) {
    outList[[i + 1]] <- str_c(
      latestData[i, ]$source, " (", format(latestData[i, ]$date, dateFormat),
      "); ")
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}

toLowerFirst <- function(string) {
  str_replace(string, ".{1}", tolower(str_extract(string, ".{1}")))
}

# colours
allCols <- viridis(6)
plotColors <-  c(
  "Confirmed cases" = allCols[1],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5])

# prepare Data
caseDataPlot <- rawData %>%
  filter(
    country == "Switzerland",
    region == "Switzerland",
    source %in% c("FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    data_type = fct_drop(data_type)
  ) %>%
  pivot_wider(names_from = "variable", values_from = "value")

estimates <- estimatesReSum %>%
  filter(
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
    estimate_type == "Cori_slidingWindow",
    between(date,
      left = estimatesDates[["Switzerland"]][["Switzerland"]][["start"]][[as.character(data_type[1])]],
      right = estimatesDates[["Switzerland"]][["Switzerland"]][["end"]][[as.character(data_type[1])]]),
  ) %>%
  ungroup()

latestDataPlot <- latestData %>%
  ungroup() %>%
  filter(
    country == "Switzerland",
    region == "Switzerland",
    source %in% unique(estimates$source)) %>%
  dplyr::select(-data_type) %>%
  distinct()

source(here::here("app", "otherScripts", "ReffPlotly.R"))

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
      source = sapply(source, translator$t,  USE.NAMES = FALSE)
    )

  plotlyPlotV <- rEffPlotly(
    caseDataPlot,
    estimates,
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

load(file = "ScriptStartTime.Rdata")

cat(paste("###", "total script duration:", signif(difftime(Sys.time(), startTime, units = "mins")), "min"))
