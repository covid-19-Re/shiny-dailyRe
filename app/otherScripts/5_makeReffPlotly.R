print(paste("starting makeReffPlotly.R:", Sys.time()))
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(here)
library(viridisLite)

# load data
dataDir <- here("app/data")
pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")
pathToRawData <- file.path(dataDir, "Raw_data.Rdata")

load(pathToRawData)
load(pathToEstimatesRePlot)
load(pathToLatestData)

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
cumulativePlotData <- rawData %>%
  filter(country == "Switzerland", region == "Switzerland",
    source %in% c("openZH", "FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    region = fct_drop(region),
    country = fct_drop(country),
    data_type = fct_drop(data_type)
  ) %>%
  pivot_wider(names_from = "variable", values_from = "value")

estimatesRePlotFiltered <- estimatesRePlot %>%
  filter(country == "Switzerland", region == "Switzerland",
    source %in% c("openZH", "FOPH"),
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    region = fct_drop(region),
    country = fct_drop(country),
    data_type = fct_drop(data_type)
  ) %>%
  filter(
    # confirmed: delay 10 days
    !(data_type == "Confirmed cases" & date > (latestData[latestData$source == "openZH", ]$date - 10)),
    # hospitalized
    !(data_type == "Hospitalized patients" & date > (latestData[latestData$source == "FOPH", ]$date - 10)),
    # deaths: delay 16 days
    !(data_type == "Deaths" & date > (latestData[latestData$source == "openZH", ]$date - 15))
  )

rEffPlotWindowData <- filter(estimatesRePlotFiltered,
  estimate_type == "Cori_slidingWindow")

latestDataPlot <- latestData %>%
  filter(country == "Switzerland",
    source %in% unique(rEffPlotWindowData$source))

source(here("app", "otherScripts", "ReffPlotly.R"))

translations <- read_csv(here("app", "data", "translations.csv"), col_types = "ccccc")
textElements <- list()
for (i in names(translations)[-1]) {
  textElements[[i]] <- translations[[i]]
  names(textElements[[i]]) <- translations$element
}

outputDir <- here("app/www")

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
    cumulativePlotData,
    rEffPlotWindowData,
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
    file.path(outputDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")

  # write_lines(
  #   htmlwidgetsExtended::exportWidgetJson(plotlyPlotV),
  #   file.path(outputDir, str_c("rEffplotly_data_", i, ".json")))
}

print(paste("Done makeReffPlotly.R:", Sys.time()))
