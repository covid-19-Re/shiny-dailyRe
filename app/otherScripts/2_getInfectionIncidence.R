cat(paste("###", Sys.time(), "- starting 2_getInfectionIncidence.R", "\n"))

library(lubridate)
library(readr)
library(utils)
library(tidyverse)
library(here)
library(fitdistrplus)
library(parallel)
library(zoo)

source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

data_dir <- here::here("app/data/temp")
raw_data_path <- file.path(data_dir, "Raw_data.rds")

infect_data_path <- file.path(data_dir, "Deconvolved_infect_data.rds")

FOPH_data_path <- here::here("app/data/CH")
delays_data_path <- file.path(FOPH_data_path, "FOPH_data_delays.csv")

delays_onset_to_count <- read_csv(delays_data_path,
                                  col_types = cols(
                                    data_type = col_character(),
                                    onset_date = col_date(format = ""),
                                    count_date = col_date(format = ""),
                                    delay = col_number()))

### Waiting time distributions ##
## hardcoded for now, but to be taken outside of script
# incubation: mean = 5.3, sd = 3.2 (Linton et al., best gamma distr fit)
mean_incubation <- 5.3
sd_incubation <- 3.2

# onset to test: pooled CH data from BAG (12/05/20 update)
mean_onset_to_test <- 4.5
sd_onset_to_test <- 4.9

# onset to hospitalization report: pooled CH data from BAG (12/05/20 update)
mean_onset_to_hosp <- 6.1
sd_onset_to_hosp <- 4.7

##TODO reconsider Excess death delay (discretize by week)
# onset to death: mean =15.0 sd=6.9 (Linton et al. best gamma distr fit)
mean_onset_to_death <- 15.0
sd_onset_to_death <- 6.9

### gamma distribution parameters for incubation period
shape_incubation <- mean_incubation^2 / (sd_incubation^2)
scale_incubation <- (sd_incubation^2) / mean_incubation

mean_onset_to_count <- c(
  "Confirmed cases" = mean_onset_to_test,
  "Deaths" = mean_onset_to_death,
  "Hospitalized patients - admission" = mean_onset_to_hosp,
  "Hospitalized patients" = mean_onset_to_hosp,
  "Excess deaths" = mean_onset_to_death)
sd_onset_to_count <- c(
  "Confirmed cases" = sd_onset_to_test,
  "Deaths" = sd_onset_to_death,
  "Hospitalized patients - admission" = sd_onset_to_hosp,
  "Hospitalized patients" = sd_onset_to_hosp,
  "Excess deaths" = sd_onset_to_death)

### parameters for gamma distribution between symptom onset and report
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2), "Hospitalized patients - onset" = 0)
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count, "Hospitalized patients - onset" = 0)


###############

raw_data <- readRDS(file = raw_data_path)

##TODO fix these issues
# manually filtering out hosp in france, deaths in spain, deaths in austria: reporting issues cause some weird Re jumps.

raw_data <- raw_data %>%
  filter(!(region %in% c("Spain", "Austria") & data_type == "Deaths")) %>%
  filter(!(region == "France" & data_type == "Hospitalized patients"))

right_truncation <- 2

raw_data <- raw_data %>% 
  group_by(country, region, source, data_type) %>% 
  filter(date <= (max(date) - right_truncation)) %>% 
  ungroup()

deconvolved_main_data <- get_all_infection_incidence(
  raw_data,
  onset_to_count_empirical_delays = delays_onset_to_count,
  # data_types = c("Confirmed cases",
  #               "Hospitalized patients",
  #               "Deaths",
  #               "Excess deaths"),
  data_types = c("Confirmed cases",
                 "Hospitalized patients",
                 "Deaths"),
  shape_incubation = shape_incubation,
  scale_incubation = scale_incubation,
  shape_onset_to_count = shape_onset_to_count,
  scale_onset_to_count = scale_onset_to_count,
  min_chi_squared = 1,
  maximum_iterations = 20,
  n_bootstrap = 50,
  verbose = F)


deconvolved_FOPH_hosp_data <- get_all_infection_incidence(
  raw_data,
  onset_to_count_empirical_delays = delays_onset_to_count,
  data_types = c("Hospitalized patients - admission",
                "Hospitalized patients - onset"),
  shape_incubation = shape_incubation,
  scale_incubation = scale_incubation,
  shape_onset_to_count = shape_onset_to_count,
  scale_onset_to_count = scale_onset_to_count,
  min_chi_squared = 1,
  maximum_iterations = 20,
  n_bootstrap = 50,
  verbose = F)

## sum infections from Hospitalized patients - admission and Hospitalized patients - onset
deconvolved_FOPH_hosp_data <- deconvolved_FOPH_hosp_data %>%
  group_by(date, country, region, data_type, source, replicate, variable) %>%
  summarise(value = sum(value)) %>%
  arrange(country, region, source, data_type, variable, replicate, date) %>%
  ungroup()

deconvolved_infections <- bind_rows(deconvolved_main_data, deconvolved_FOPH_hosp_data) %>% ungroup()

saveRDS(deconvolved_infections, file = infect_data_path)

###############

cat(paste("###", Sys.time(), "- done 2_getInfectionIncidence.R", "\n"))
