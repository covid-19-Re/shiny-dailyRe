print(paste("starting makeReffPlotly.R:", Sys.time()))
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(here)
library(viridisLite)

# load data
dataDir <- here("app/data")
pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathTolastDataDate <- file.path(dataDir, "lastDataDate.Rdata")
pathToRawData <- file.path(dataDir, "Raw_data.Rdata")

load(pathToRawData)
load(pathToEstimatesRePlot)
load(pathTolastDataDate)

# common functions
dataUpdatesString <- function(lastDataDate, name = "Data Source") {
  outList <- list(str_c(name, ": "))
  for (i in 1:dim(lastDataDate)[1]) {
    outList[[i+1]] <- str_c(
      lastDataDate[i,]$source, " (", as.character(lastDataDate[i,2]$date),
      "); ")
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}

# colours
allCols <- viridis(6)
plotColoursNamed <-  c(
  "Confirmed cases" = allCols[1],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5])

# prepare Data
cumulativePlotData <- filter(rawData, !(region != "CH" & data_type == "deaths")) %>%
  mutate(data_type = factor(
    data_type,
    levels = c("confirmed", "hospitalized", "deaths"),
    labels = c("Confirmed cases", "Hospitalized patients", "Deaths"))) %>%
  pivot_wider(names_from = "variable", values_from = "value")

caseData <- cumulativePlotData %>% filter(region == "CH")

startDate <- min(caseData$date) - 1
endDate <- Sys.Date()
lastDate <- max(caseData$date)
maxEstimateDate <- max(caseData$date) - 10
minEstimateDate <- as.Date("2020-03-07")

estimatesRePlotFiltered <- filter(estimatesRePlot,
    # confirmed: delay 10 days
    !(data_type == "Confirmed cases" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 10)),
    # hospitalized
    !(data_type == "Hospitalized patients" & date > (lastDataDate[lastDataDate$source == "FOPH",]$date - 10)),
    # deaths: delay 16 days
    !(data_type == "Deaths" & date > (lastDataDate[lastDataDate$source == "openZH",]$date - 15))
  )

rEffPlotWindowData <- filter(estimatesRePlotFiltered,
  estimate_type == "Cori_slidingWindow")

source(here("app", "otherScripts", "ReffPlotly.R"))

translations <- read_csv(here("app", "data", "translations.csv"), col_types = "ccccc")
textElements <- list()
for (i in names(translations)[-1]){
  textElements[[i]] <- translations[[i]]
  names(textElements[[i]]) <- translations$element
}

outputDir <- here("app/www")

# i <- names(textElements)[3]

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
    lastDataDate,
    legendOrientation = "v",
    textElements = textElements,
    language = i,
    widgetID = "rEffplots")

  plotlyPlotV$sizingPolicy$browser$padding <- 0

  plotlyPlotV

  htmlwidgets::saveWidget(plotlyPlotV,
    file.path(outputDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")

  write_lines(
    htmlwidgetsExtended::exportWidgetJson(plotlyPlotV),
    file.path(outputDir, str_c("rEffplotly_data_", i, ".json")))
}

print(paste("Done makeReffPlotly.R:", Sys.time()))
