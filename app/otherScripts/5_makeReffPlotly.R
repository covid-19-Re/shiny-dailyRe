print(paste("starting makeReffPlotly.R:", Sys.time()))
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(here)
library(viridisLite)

# load data
dataDir <- here("app/data/temp")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")
pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToEstimateDates <- file.path(dataDir, "estimate_dates.Rdata")

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
plotColoursNamed <-  c(
  "Confirmed cases" = allCols[1],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5])

# prepare Data
caseDataPlot <- rawData %>%
  filter(country == "Switzerland",
    source %in% c("openZH", "FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    data_type = fct_drop(data_type)
  ) %>%
  pivot_wider(names_from = "variable", values_from = "value")

estimates <- estimatesReSum %>%
  filter(country == "Switzerland",
    source %in% c("openZH", "FOPH"),
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
      left = estimatesDates[["Switzerland"]][["start"]][[as.character(data_type[1])]],
      right = estimatesDates[["Switzerland"]][["end"]][[as.character(data_type[1])]]),
  ) %>%
  ungroup()

latestDataPlot <- latestData %>%
  filter(country == "Switzerland",
    source %in% unique(estimates$source))

source(here("app", "otherScripts", "ReffPlotly.R"))

translations <- read_csv(here("app", "data", "translations.csv"), col_types = "ccccc")
textElements <- list()
for (i in names(translations)[-1]) {
  textElements[[i]] <- translations[[i]]
  names(textElements[[i]]) <- translations$element
}

plotOutDir <- here("app/www")

for (i in names(textElements)) {
  pathToInterventionData <- here(str_c(
    "../ch-hospital-data/data/interventions_", i, ".csv"))

  interventions <- read_csv(pathToInterventionData,
    col_types = cols(
      name = col_character(),
      y = col_double(),
      text = col_character(),
      tooltip = col_character(),
      type = col_character(),
      date = col_date(format = "")))

  plotlyPlotV <- rEffPlotly(
    caseDataPlot,
    estimates,
    interventions,
    plotColoursNamed,
    lastDataDate = latestDataPlot,
    legendOrientation = "v",
    textElements = textElements,
    language = i,
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

print(paste("Done makeReffPlotly.R:", Sys.time()))
