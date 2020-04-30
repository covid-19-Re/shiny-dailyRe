library(tidyverse)

dataDir <- "data"

pathToRawData <- file.path(dataDir, "Raw_data.Rdata")
pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesRePlot <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToCantonList <- file.path(dataDir, "cantonList.Rdata")
pathTolastDataDate <- file.path(dataDir, "lastDataDate.Rdata")

load(pathToCantonList)

# helpers

dataUpdatesTable <- function(lastDataDate){
  outList <- list("<table style=\"width:100%\">")
  for (i in 1:dim(lastDataDate)[1]) {
    outList[[i + 1]] <- str_c(
      "<tr><td>",
      lastDataDate[i,]$source, "</td><td>", as.character(lastDataDate[i,2]$date),
      "</td></tr>")
  }
  outList[[i + 2]] <- "</table>"
  return(str_c(outList, collapse = ""))
}

dataUpdatesString <- function(lastDataDate){
  outList <- list("Data Source: ")
  for (i in 1:dim(lastDataDate)[1]) {
    outList[[i+1]] <- str_c(
      lastDataDate[i,]$source, " (", as.character(lastDataDate[i,2]$date),
      "); ")
  }
  return(str_sub(str_c(outList, collapse = ""), end = -3))
}

ggColor <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
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

plotColoursCases <- c("confirmed" = "black", "hospitalized" = "#619CFF", "deaths" = "#F8766D")
plotColoursEstimates <- c("infection_confirmed" = "black", "infection_hospitalized" = "#619CFF", "infection_deaths" = "#F8766D")

colourScaleCases <- scale_colour_manual(
        values = plotColoursCases,
        name  = "Data source",
        labels = c("Confirmed cases", "Hospitalizations", "Deaths"),
        aesthetics = c("colour", "fill"))

colourScaleEstimates <- scale_colour_manual(
        values = plotColoursEstimates,
        labels = c("infection_confirmed" = "Confirmed cases", "infection_hospitalized" = "Hospitalizations", "infection_deaths" = "Deaths"),
        name = "Data source",
        aesthetics = c("fill", "color"))
