# format estimate data in seperate process to avoid c stack limit
cat(paste("###", Sys.time(), "- starting 4_doReEstimatesSum.R", "\n"))
library(tidyverse)
library(here)

#############################
dataDir <- here("app/data/temp")

pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")
pathToValidEstimates <- file.path(dataDir, "valid_estimates.Rdata")


load(file = pathToEstimatesReRaw)
load(file = pathToLatestData)
load(file = pathToValidEstimates)

#############################
estimatesRe <- as_tibble(estimatesReRaw) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  filter(
    country %in% validEstimates$country,
    region %in% validEstimates$region,
  )

save(estimatesRe, file = pathToEstimatesRe)

#############################

estimatesReSum <- estimatesRe %>%
  filter(
    # exclude infection_deaths in Cantons
    !(country == "Switzerland" & region != "Switzerland" & data_type == "Deaths")) %>%
  group_by(date, country, region, data_type, source, estimate_type) %>%
  summarize(
    median_R_mean = median(R_mean),
    median_R_highHPD = median(R_highHPD),
    median_R_lowHPD = median(R_lowHPD)
  ) %>%
  select(country, region, source, data_type, estimate_type, date, median_R_mean, median_R_highHPD, median_R_lowHPD) %>%
  arrange(country, region, source, data_type, estimate_type, date) %>%
  ungroup()

save(estimatesReSum, file = pathToEstimatesReSum)

#############################
cat(paste("###", Sys.time(), "- done 4_doReEstimatesSum.R", "\n"))
