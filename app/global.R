library(tidyverse)
library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)

source("otherScripts/ReffPlotly.R")

dataDir <- "data"

translator <- Translator$new(translation_json_path = file.path(dataDir, "shinyTranslations.json"))
languageSelect <- translator$languages
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToCantonList <- file.path(dataDir, "cantonList.Rdata")
pathTolatestData <- file.path(dataDir, "latestData.Rdata")
pathToLastCheck <- file.path(dataDir, "lastCheck.txt")
pathToInterventionData <- file.path("../../ch-hospital-data/data/interventions")
pathToTranslations <- file.path(dataDir, "translations.csv")
load(pathToCantonList)

translations <- read_csv(here("app", "data", "translations.csv"), col_types = "ccccc")
textElements <- list()
for (i in names(translations)[-1]) {
  textElements[[i]] <- translations[[i]]
  names(textElements[[i]]) <- translations$element
}

# helpers

dataUpdatesTable <- function(latestData, lastCheck, dateFormat = "%Y-%m-%d") {
  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(latestData)[1]) {
    outList[[i + 1]] <- str_c(
      "<tr><td>",
      latestData[i, ]$source, "</td><td>", format(latestData[i, ]$date, dateFormat),
      "</td></tr>")
  }
  outList[[i + 2]] <- "</table>"
  out <- str_c(outList, collapse = "")
  out <- str_c(out, "<small style=\"font-weight: normal\">", lastCheck, "</small>")
  return(out)
}

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
  "Deaths" = allCols[5],
  "Excess Deaths" = allCols[6])

plotColoursNamedT <-  c(
  "Confirmed cases" = allColsTransparent[1],
  "Hospitalized patients" = allColsTransparent[3],
  "Deaths" = allColsTransparent[5],
  "Excess Deaths" = allCols[6])

colorScale <- scale_colour_manual(
        values = plotColoursNamed,
        name  = "",
        aesthetics = c("colour", "fill"))
