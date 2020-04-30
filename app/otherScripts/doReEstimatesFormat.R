# format estimate data in seperate process to avoid c stack limit
print(paste("starting doReEstimatesFormat.R: ", Sys.time()))
library(tidyverse)
library(here)

dataDir <- here("app/data")

pathToEstimatesReRawSave <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesReSave <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesRePlotSave <- file.path(dataDir, "Estimates_Re_plot.Rdata")
pathToCantonListSave <- file.path(dataDir, "cantonList.Rdata")
pathTolastDataDateSave <- file.path(dataDir, "lastDataDate.Rdata")

load(file = pathToEstimatesReRawSave)
load(file = pathToCantonListSave)
load(file = pathTolastDataDateSave)

estimatesRe <- as_tibble(estimatesReRaw) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(
    replicate = as_factor(replicate),
    data_type = factor(data_type, levels = c("infection_deaths", "infection_hospitalized","infection_confirmed")),
    region = factor(region, levels = cantonList))

save(estimatesRe, file = pathToEstimatesReSave)

estimatesRePlot <- estimatesRe %>%
  filter(
    # exclude infection_deaths in CH
    !(region != "CH" & data_type == "infection_deaths"),
    # exclude infection deaths newer than 16 days ago
    !(data_type == "infection_deaths" & date > (Sys.Date() - 16)),
    # exclude infection hospitalized data not provided by BAF
    !(data_type == "infection_hospitalized" & date > filter(lastDataDate, source == "BAG")$date))

estimatesRePlot$median_R_mean <- with(estimatesRePlot,
  ave(R_mean, date, region, data_type, source, estimate_type,
    FUN = median))
estimatesRePlot$median_R_highHPD <- with(estimatesRePlot,
  ave(R_highHPD, date, region, data_type, source, estimate_type,
    FUN = median))
estimatesRePlot$median_R_lowHPD <- with(estimatesRePlot,
  ave(R_lowHPD, date, region, data_type, source, estimate_type,
    FUN = median))
  
estimatesRePlot$highQuantile_R_highHPD <- with(estimatesRePlot,
  ave(R_highHPD, date, region, data_type, source, estimate_type,
    FUN = function(x) quantile(x, probs = 0.975, na.rm=T)))
estimatesRePlot$lowQuantile_R_lowHPD <- with(estimatesRePlot,
  ave(R_lowHPD, date, region, data_type, source, estimate_type,
  FUN = function(x) quantile(x, probs = 0.025, na.rm = T)))

save(estimatesRePlot, file = pathToEstimatesRePlotSave)

print(paste("Done doReEstimatesFormat.R: ", Sys.time()))
