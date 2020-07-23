if (interactive()) {
  library(plotly)
  library(here)
  library(shiny.i18n)
  library(htmlwidgets)
  library(tidyverse)
} else {
  suppressPackageStartupMessages({
    library(plotly)
    library(here)
    library(shiny.i18n)
    library(htmlwidgets)
    library(tidyverse)
  })
}

source(here("app", "otherScripts", "ReffPlotly.R"))
source(here("app", "utils.R"))

iso3 <- "CHE"
cat(str_c("making ", iso3, " plots for ncs-tf website ...\n"))

# load data
dataDir <- here("app/data")
plotOutDir <- here("app/www")
pathToCaseData <- file.path(dataDir, str_c("countryData/Europe/", iso3, "-Data.rds"))
pathToEstimates <- file.path(dataDir, str_c("countryData/Europe/", iso3, "-Estimates.rds"))
pathToUpdateData <- file.path(dataDir, "updateData.rds")
pathToInterventionData <- here("../covid19-additionalData/interventions/interventions.csv")

caseData <- readRDS(pathToCaseData) %>%
  pivot_wider(names_from = "variable", values_from = "value")
estimates <- readRDS(pathToEstimates)
updateData <- readRDS(pathToUpdateData)[[iso3]]

# prepare Data
caseDataPlot <- caseData %>%
  filter(
    countryIso3 == iso3,
    region == iso3,
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(data_type = fct_drop(data_type))

estimatePlotRanges <- estimateRanges(caseDataPlot,
  minConfirmedCases = 100,
  delays = delaysDf)

estimatesPlot <- estimates %>%
  filter(
    estimate_type == "Cori_slidingWindow",
    countryIso3 == iso3,
    region == iso3,
    data_type %in% c("Confirmed cases", "Hospitalized patients", "Deaths")) %>%
  mutate(
    region = fct_drop(region),
    country = fct_drop(country),
    data_type = fct_drop(data_type)
  ) %>%
  group_by(data_type) %>%
  filter(
    between(date,
      left = estimatePlotRanges[[iso3]][[iso3]][["start"]][[as.character(data_type[1])]],
      right = estimatePlotRanges[[iso3]][[iso3]][["end"]][[as.character(data_type[1])]]),
  ) %>%
  ungroup()

updateDataPlot <- updateData %>%
  ungroup() %>%
  filter(
    countryIso3 == iso3,
    region == iso3,
    source %in% unique(estimates$source)) %>%
  distinct()

translator <- Translator$new(translation_json_path = here("app", "data", "shinyTranslations.json"))

interventions <- read_csv(file = pathToInterventionData,
  col_types = cols(
      .default = col_character(),
      date = col_date(format = ""),
      y = col_double())) %>%
  filter(countryIso3 == iso3)


for (i in translator$languages) {

  translator$set_translation_language(i)

  interventionsLocalized <- interventions %>%
      mutate(
        text = sapply(text, translator$t,  USE.NAMES = FALSE),
        tooltip =  sapply(tooltip, translator$t,  USE.NAMES = FALSE))

  updateDataPlotLocalized <- updateDataPlot %>%
    mutate(
      source = sapply(source, translator$t,  USE.NAMES = FALSE),
      data_type = sapply(as.character(data_type), translator$t,  USE.NAMES = FALSE)
    )

  plotlyPlotV <- rEffPlotly(
    caseDataPlot,
    estimatesPlot,
    interventionsLocalized,
    plotColors,
    lastDataDate = updateDataPlotLocalized,
    fixedRangeX = c(TRUE, TRUE, TRUE),
    fixedRangeY = c(TRUE, TRUE, TRUE),
    language = i,
    translator = translator,
    widgetID = "rEffplots")

  plotlyPlotV$sizingPolicy$browser$padding <- 0

  plotlyPlotV

  saveWidget(plotlyPlotV,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")
}

cat("done making CH plots for ncs-tf website.\n")
