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

source(here::here("app", "otherScripts", "plotlyFunctions.R"))
source(here::here("app", "utils.R"))

countrySelectValue <- "CHE"
cat(str_c("making ", countrySelectValue, " plots for ncs-tf website ...\n"))

# load data
dataDir <- here::here("app/data")
plotOutDir <- here::here("app/www")

pathToCountryData <- file.path(dataDir, "serialized", "allCountryData.qs")
pathToUpdateData <- file.path(dataDir, "serialized", "updateDataRaw.qs")
pathToInterventionData <- here::here("../covid19-additionalData/interventions/interventions.csv")
pathToContinentsData <- file.path(dataDir, "continents.csv")
continents <- read_csv(pathToContinentsData, col_types = cols(.default = col_character()))

allData <- qs::qread(pathToCountryData)
countryData <- list(
  caseData = filter(allData$caseData, countryIso3 %in% countrySelectValue),
  estimates = filter(allData$estimates, countryIso3 %in% countrySelectValue)
)

updateDataRaw <-  qs::qread(pathToUpdateData)
updateData <- bind_rows(updateDataRaw[countrySelectValue]) %>%
  ungroup() %>%
  dplyr::select(-country) %>%
  left_join(dplyr::select(continents, countryIso3, country), by = "countryIso3")

interventions <- read_csv(
  str_c(pathToInterventionData),
  col_types = cols(
    .default = col_character(),
    date = col_date(format = ""),
    y = col_double()
  )) %>%
  split(f = .$countryIso3)

translator <- Translator$new(translation_json_path = file.path(dataDir, "covid19reTranslations.json"))
availableLanguages <- translator$get_languages()

rightTruncation <- list(
  "CHE" = list(
    "Confirmed cases" = 3,
    "Confirmed cases / tests" = 3,
    "Hospitalized patients" = 5,
    "Deaths" = 5)
)

for (i in availableLanguages) {

  translator$set_translation_language(i)

  plot <- rEffPlotlyShiny(
    countryData,
    updateData,
    interventions,
    seriesSelect = "data_type",
    input = list(
      estimationTypeSelect = "Cori_slidingWindow",
      plotOptions = c("none"),
      caseAverage = 1,
      lang = i),
    rightTruncation,
    translator,
    plotSize = "small",
    showHelpBox = FALSE)

  plot$sizingPolicy$browser$padding <- 0

  saveWidget(plot,
    file.path(plotOutDir, str_c("rEffplotly_", i, ".html")), selfcontained = FALSE, libdir = "lib",
    title = "Effective reproductive number (Re) in Switzerland")
}


# eth website plots

timeSpan <- 21

plotTheme <- theme_bw() +
  theme(
    # overall text size
    text = element_text(size = 8),
    # legend
    legend.position = "right",
    legend.key = element_rect(color = NA),
    # panel
    panel.border = element_rect(color = NA),
    panel.grid.major = element_line(size = 0.2),
    panel.grid.minor = element_line(size = 0.2),
    # axis
    axis.line = element_line(size = 0.2),
    axis.ticks = element_line(size = 0.2),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  )

allCols <- viridis(6)
plotColors <-  c(
  "Confirmed cases" = allCols[1],
  "Confirmed cases / tests" = allCols[2],
  "Hospitalized patients" = allCols[3],
  "Deaths" = allCols[5],
  "Excess deaths" = allCols[6])


translator$set_translation_language("en-gb")

yAxisTitle <- bquote(Reproductive~number~R[e])

regionLoop <- countryData$estimates %>%
  filter(str_detect(region, "grR", negate = TRUE)) %>%
  pull(region) %>%
  unique()

widthCHE <- 800 / 150
heightCHE <- 400 / 150
widthCanton <- widthCHE / 2
heightCanton <- heightCHE * 0.75


for (iregion in regionLoop) {
  staticPlotData <- countryData$estimates %>%
    filter(
      data_type == "Confirmed cases",
      estimate_type == "Cori_slidingWindow",
      region == iregion,
    ) %>%
  filter(date > max(date) - (timeSpan + 2))

  xLimits <- c(max(staticPlotData$date) - timeSpan, max(staticPlotData$date))

  lastDataDate <- updateData %>%
    filter(
      data_type == "Confirmed cases",
      region == iregion,
    ) %>%
    pull(lastData) %>%
    format(translator$t("%Y-%m-%d"))

  plotCaption <- str_c(
    translator$t("Last Data Updates"), ": ",
    lastDataDate
  )

  plotTitle <- if_else(staticPlotData$region[1] == "CHE", translator$t("Switzerland"), staticPlotData$region[1])

  yAxisBreaks <- staticPlotData$date[c(1, 7, 14, 21)+2]


  plot <- ggplot(
    data = staticPlotData,
    mapping = aes(x = date, y = median_R_mean, ymin = median_R_lowHPD, ymax = median_R_highHPD,
      label = round(median_R_mean, 2))) +
    geom_ribbon(alpha = 0.1, fill = allCols[1]) +
    geom_line(color = allCols[1]) +
    # geom_text(
    #   data = filter(staticPlotData, date == max(date)),
    #   # mapping = aes(x = date, y = 0.1),
    #   hjust = 0, vjust = 0.5, nudge_x = 0.1,
    #   size = 1.8
    # ) +
    scale_x_date(name = NULL, breaks = yAxisBreaks, date_minor_breaks = "1 day") +
    scale_y_continuous(name = yAxisTitle) +
    coord_cartesian(ylim = c(0, 2), xlim = xLimits) +
    geom_hline(yintercept = 1, linetype = 2) +
    labs(
      title = plotTitle,
      caption = if_else(iregion == "CHE", plotCaption, "")) +
    plotTheme

  ggsave(
    filename = file.path(plotOutDir, "cantonPlots", str_c(iregion, availableLanguages[1], "rePlot.png", sep = "_")),
    width = if_else(iregion == "CHE", widthCHE, widthCanton),
    height = if_else(iregion == "CHE", heightCHE, heightCanton),
    units = "in",
    dpi = 300)
}

cat("done making CH plots for ncs-tf website.\n")
