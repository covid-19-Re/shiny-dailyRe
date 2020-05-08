# format estimate data in seperate process to avoid c stack limit
print(paste("starting 4_doReEstimatesSum.R: ", Sys.time()))
library(tidyverse)
library(here)

#############################
dataDir <- here("app/data/temp")

pathToEstimatesReRaw <- file.path(dataDir, "Estimates_Re_raw.Rdata")
pathToEstimatesRe <- file.path(dataDir, "Estimates_Re.Rdata")
pathToEstimatesReSum <- file.path(dataDir, "Estimates_Re_sum.Rdata")
pathToCantonList <- file.path(dataDir, "cantonList.Rdata")
pathToLatestData <- file.path(dataDir, "latestData.Rdata")


load(file = pathToEstimatesReRaw)
load(file = pathToLatestData)

#############################
estimatesRe <- as_tibble(estimatesReRaw) %>%
  pivot_wider(names_from = "variable", values_from = "value")

save(estimatesRe, file = pathToEstimatesRe)

#############################

estimatesReSum <- estimatesRe %>%
  filter(
    # exclude infection_deaths in CH
    !(country == "Switzerland" & region != "Switzerland" & data_type == "Deaths"),
    # exclude infection hospitalized data not provided by FOPH
    !(country == "Switzerland" & data_type == "Hospitalized patients" &
      date > filter(latestData, source == "FOPH")$date))

estimatesReSum$median_R_mean <- with(estimatesReSum,
  ave(R_mean, date, country, region, data_type, source, estimate_type, FUN = median))
estimatesReSum$median_R_highHPD <- with(estimatesReSum,
  ave(R_highHPD, date, country, region, data_type, source, estimate_type, FUN = median))
estimatesReSum$median_R_lowHPD <- with(estimatesReSum,
  ave(R_lowHPD, date, country, region, data_type, source, estimate_type, FUN = median))

# lets save processing as long as we don't use it
#
# estimatesReSum$highQuantile_R_highHPD <- with(estimatesReSum,
#   ave(R_highHPD, date, country, region, data_type, source, estimate_type,
#     FUN = function(x) quantile(x, probs = 0.975, na.rm = TRUE)))
# estimatesReSum$lowQuantile_R_lowHPD <- with(estimatesReSum,
#   ave(R_lowHPD, date, country, region, data_type, source, estimate_type,
#     FUN = function(x) quantile(x, probs = 0.025, na.rm = TRUE)))

# remove replicates and individual estimates
estimatesReSum <- estimatesReSum %>%
  filter(replicate == 1) %>%
  select(-replicate, -R_mean, -R_highHPD, -R_lowHPD) 

save(estimatesReSum, file = pathToEstimatesReSum)

#############################
print(paste("Done 4_doReEstimatesSum.R: ", Sys.time()))
