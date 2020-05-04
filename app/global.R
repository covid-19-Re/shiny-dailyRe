library(tidyverse)
library(plotly)
library(viridisLite)
library(here)

source("otherScripts/ReffPlotly.R")

dataDir <- "data"

pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToCantonList <- file.path(dataDir, "cantonList.Rdata")
pathTolastDataDate <- file.path(dataDir, "lastDataDate.Rdata")
pathToLastCheck <- file.path(dataDir, "lastCheck.txt")
pathToInterventionData <- file.path("../../ch-hospital-data/data/interventions_en-gb.csv")
pathToTranslations <- file.path(dataDir, "translations.csv")
load(pathToCantonList)

translations <- read_csv(here("app", "data", "translations.csv"), col_types = "ccccc")
textElements <- list()
for (i in names(translations)[-1]){
  textElements[[i]] <- translations[[i]]
  names(textElements[[i]]) <- translations$element
}

# helpers

dataUpdatesTable <- function(lastDataDate, lastCheck){
  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(lastDataDate)[1]) {
    outList[[i + 1]] <- str_c(
      "<tr><td>",
      lastDataDate[i,]$source, "</td><td>", as.character(lastDataDate[i,2]$date),
      "</td></tr>")
  }
  outList[[i + 2]] <- "</table>"
  out <- str_c(outList, collapse = "")
  out <- str_c(out, "<small style=\"font-weight: normal\">", lastCheck, "</small>")
  return(out)
}

dataUpdatesString <- function(lastDataDate, name = "Data Source") {
  outList <- list(str_c(name, ": "))
  for (i in 1:dim(lastDataDate)[1]) {
    outList[[i+1]] <- str_c(
      lastDataDate[i,]$source, " (", as.character(lastDataDate[i,2]$date),
      "); ")
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}

plotTheme <- theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
    axis.title.y =  element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

allCols <- viridis(6)
allColsTransparent <- viridis(6, 0.5)

plotColoursNamed <-  c(
  "Confirmed cases" = allCols[1],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5])

plotColoursNamedT <-  c(
  "Confirmed cases" = allColsTransparent[1],
  "Hospitalized patients" = allColsTransparent[3],
  "Deaths" = allColsTransparent[5])

colorScale <- scale_colour_manual(
        values = plotColoursNamed,
        name  = "",
        aesthetics = c("colour", "fill"))

