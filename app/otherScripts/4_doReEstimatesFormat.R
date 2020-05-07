# format estimate data in seperate process to avoid c stack limit
print(paste("starting doReEstimatesFormat.R: ", Sys.time()))
library(tidyverse)
library(here)

#############################
dataDir <- here("app/data")

pathToEstimatesReRawSave <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesReSave <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesRePlotSave <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToCantonListSave <- file.path(dataDir, "cantonList.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")


load(file = pathToEstimatesReRawSave)
load(file = pathToLatestData)

#############################
estimatesRe <- as_tibble(estimatesReRaw) %>%
  pivot_wider(names_from = "variable", values_from = "value")

save(estimatesRe, file = pathToEstimatesReSave)

#############################

estimatesRePlot <- estimatesRe %>%
  filter(
    # exclude infection_deaths in CH
    !(country == "Switzerland" & region != "Switzerland" & data_type == "Deaths"),
    # exclude infection hospitalized data not provided by FOPH
    !(country == "Switzerland" & data_type == "Hospitalized patients" &
      date > filter(latestData, source == "FOPH")$date))

estimatesRePlot$median_R_mean <- with(estimatesRePlot,
  ave(R_mean, date, country, region, data_type, source, estimate_type, FUN = median))
estimatesRePlot$median_R_highHPD <- with(estimatesRePlot,
  ave(R_highHPD, date, country, region, data_type, source, estimate_type, FUN = median))
estimatesRePlot$median_R_lowHPD <- with(estimatesRePlot,
  ave(R_lowHPD, date, country, region, data_type, source, estimate_type, FUN = median))

# lets save processing as long as we don't use it
#
# estimatesRePlot$highQuantile_R_highHPD <- with(estimatesRePlot,
#   ave(R_highHPD, date, country, region, data_type, source, estimate_type,
#     FUN = function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
# estimatesRePlot$lowQuantile_R_lowHPD <- with(estimatesRePlot,
#   ave(R_lowHPD, date, country, region, data_type, source, estimate_type,
#     FUN = function(x) quantile(x, probs = 0.025, na.rm = TRUE)))

save(estimatesRePlot, file = pathToEstimatesRePlotSave)


#############################
print(paste("Done doReEstimatesFormat.R: ", Sys.time()))
