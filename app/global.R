library(tidyverse)
library(plotly)
library(viridisLite)
library(here)
library(shiny.i18n)
library(slider)
library(shades)

source("otherScripts/ReffPlotly.R")

# source for 
source("otherScripts/2_utils_getInfectionIncidence.R")

dataDir <- "data"

translator <- Translator$new(translation_json_path = file.path(dataDir, "shinyTranslations.json"))
languageSelect <- translator$languages
names(languageSelect) <- c("EN", "IT", "FR", "DE")

pathToRawData <- file.path(dataDir, "Raw_data.rds")
pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.rds")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.rds")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.rds")
pathTolatestData <- file.path(dataDir, "latestData.rds")
pathToLastCheck <- file.path(dataDir, "lastCheck.txt")
pathToInterventionData <- file.path("../../covid19-additionalData/interventions/")
pathToCountryList <- file.path(dataDir, "countryList.rds")
pathToEstimatesDates <- file.path(dataDir, "estimate_dates.rds")
pathToValidEstimates <- file.path(dataDir, "valid_estimates.rds")
infection_data_file_path <- file.path(dataDir,  "Deconvolved_infect_data.rds")

pathToPopSizes <- file.path(dataDir, "popSizes.Rdata")

countryList <- readRDS(pathToCountryList)

# helpers

dataUpdatesTable <- function(
  latestData,
  lastCheck,
  dateFormat = "%Y-%m-%d",
  showDataType = FALSE) {

  latestData <- latestData %>%
    group_by(country, source) %>%
    slice(1L)
  showCountry <- length(unique(latestData$country)) > 1

  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(latestData)[1]) {
    if (i == 1) {
      printCountry <- showCountry
    } else {
      printCountry <- (showCountry & latestData[i - 1, ]$country != latestData[i, ]$country)
    }

    if (printCountry) {
      countryString <- str_c("<tr><td colspan=\"3\">", latestData[i, ]$country, "</td></tr>")
    } else {
      countryString <- ""
    }

    sourceString <- str_c("<td style = \"font-weight: normal;\">", latestData[i, ]$source, "</td>")
    if (showCountry) {
      sourceString <- str_c("<td>&nbsp;&nbsp;</td>", sourceString)
    }

    outList[[i + 1]] <- str_c(
      countryString,
      "<tr>",
        sourceString,
        "<td style = \"font-weight: normal;font-style: italic;\">", format(latestData[i, ]$date, dateFormat), "</td>",
      "</tr>")
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
allCols <- viridis(6)

plotColors <-  c(
  "Confirmed cases" = allCols[1],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5],
  "Excess deaths" = allCols[6])

plotColorsTruncated <- saturation(plotColors, value = 0.1)
names(plotColorsTruncated) <- str_c(names(plotColors), " truncated")

plotColors <- c(plotColors, plotColorsTruncated)

fixedRangeX <- c(FALSE, FALSE, FALSE)
fixedRangeY <- c(TRUE, TRUE, TRUE)
