library(tidyverse)
library(plotly)
library(here)
library(viridisLite)

# load data
dataDir <- here("app/data")

pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathTolastDataDate <- file.path(dataDir, "lastDataDate.Rdata")
pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToInterventionData <- here("../ch-hospital-data/data/interventions.csv")


load(pathToRawData)
load(pathToEstimatesRePlot)
load(pathTolastDataDate)

# common functions

dataUpdatesString <- function(lastDataDate){
  outList <- list("Data Source: ")
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

interventions <- read_csv(pathToInterventionData,
  col_types = cols(
    name = col_character(),
    y = col_double(),
    text = col_character(),
    tooltip = col_character(),
    type = col_character(),
    date = col_date(format = "")))

source(here("app", "otherScripts", "ReffPlotly.R"))

plot <- rEffPlotly(
  cumulativePlotData,
  rEffPlotWindowData,
  interventions,
  plotColoursNamed,
  lastDataDate,
  widgetID = "rEffplots")

outputDir <- here("app/www")
#htmlwidgets::saveWidget(plot, file.path(outputDir,"rEffplotly_selfContained.html"), selfcontained = TRUE, libdir = "lib")
#htmlwidgets::saveWidget(plot, file.path(outputDir,"rEffplotly.html"), selfcontained = FALSE, libdir = "lib")
write_lines(htmlwidgetsExtended::exportWidgetJson(plot), file.path(outputDir,"rEffplotly_data.json"))


