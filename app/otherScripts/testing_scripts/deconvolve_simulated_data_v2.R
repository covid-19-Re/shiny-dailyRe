rm(list=ls())
library("tidyverse")
library("readr")
library("lubridate")
library("reshape2")

### Help function for plotting
gg_color <- function(n) {
  
  hues = seq(15, 375, length = n + 1)
  
  hcl(h = hues, l = 65, c = 100)[1:n]
  
}

library(lubridate)
library(readr)
library(utils)
library(tidyverse)
library(here)
library(fitdistrplus)
library(parallel)

source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

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
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2), "Hospitalized patients - onset" = 0, "Symptoms" = 0)
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count, "Hospitalized patients - onset" = 0, "Symptoms" = 0)


constant_delay_distributions <- list()
for(type_i in unique(names(shape_onset_to_count) )) {
  m <- get_vector_constant_waiting_time_distr(
    shape_incubation,
    scale_incubation,
    shape_onset_to_count[[type_i]],
    scale_onset_to_count[[type_i]])
  
  constant_delay_distributions <- c(constant_delay_distributions, list(m))
}
names(constant_delay_distributions) <- unique(names(shape_onset_to_count))

###############


dataDir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/Simulation/Jana"
outputDir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/data/Simulation/Validation_deconvolution"
plotDir <- "/Users/scirej/Documents/nCov19/Incidence_analysis/Plots/simulations_deconvolution_v2"

simulated_data <- read_csv(file=file.path(dataDir,  "deconv_sim_symptoms.csv"))

confirmed_cases <- simulated_data %>%
  filter(type == "Simulated Cases") %>%
  dplyr::rename(data_type = type)

infections <- simulated_data %>%
  filter(type == "Original") %>%
  dplyr::select(-type)

final_estimate <- get_infection_incidence_by_deconvolution(confirmed_cases %>%
                                                             mutate(region = NA, country = NA, source = NA, data_type = "Symptoms"),
                                         smooth_incidence = F,
                                         n_bootstrap = 0,
                                         max_iterations = 1000,
                                         constant_delay_distribution = constant_delay_distributions[["Symptoms"]],
                                         days_further_in_the_past = 30,
                                         verbose = T)

infections_deconvolved <- dplyr::select(final_estimate, date, value, data_type) %>% mutate ( data_type = "Infections - deconvolved") 

### output results
infections_simulated <- infections %>% mutate ( data_type = "Infections - simulated") 

output_results <- rbind(infections_deconvolved,
                        infections_simulated,
                        confirmed_cases)

# write_csv(output_results, path=file.path(outputDir, paste0("Simulated_deconvoluted_symptoms_v2.csv")))

ggplot(output_results) +
  geom_line(mapping = aes(x=date, y=value, colour=data_type), size = 1.5) + 
  coord_cartesian(ylim = c(0,2100)) +
  scale_colour_manual(name="", 
                      values=c(gg_color(3)[c(3,1)], "black"),
                      breaks = c( "Simulated Cases", 
                                  "Infections - deconvolved",
                                  "Infections - simulated"),
                      labels=c("Reports - simulated", "Infections - deconvolved", "Infections - simulated")) +
  scale_x_date(breaks="5 days", 
               date_labels = '%b\n%d',
               limits=c(as.Date("2020-05-01"), as.Date("2020-06-10"))) +
  ylab("Incidence") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text = element_text(size=15),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12)) 

ggsave(filename = file.path(plotDir, "Deconvolution_simulated_symptoms_100_iterations.png"))

confirmed_cases_truncated <- simulated_data %>%
  filter(type == "Simulated Cases") %>%
  dplyr::rename(data_type = type) %>% 
  filter(date > as.Date("2020-05-24"))

final_estimate_truncated <- get_infection_incidence_by_deconvolution(confirmed_cases_truncated %>%
                                                             mutate(region = NA, country = NA, source = NA, data_type = "Symptoms"),
                                                           smooth_incidence = F,
                                                           n_bootstrap = 0,
                                                           constant_delay_distribution = constant_delay_distributions[["Symptoms"]],
                                                           verbose = T)

infections_deconvolved_truncated <- dplyr::select(final_estimate_truncated, date, value, data_type) %>% mutate ( data_type = "Infections - deconvolved") 

### output results
infections_simulated <- infections %>% mutate ( data_type = "Infections - simulated") 

output_results_truncated <- rbind(infections_deconvolved_truncated,
                        infections_simulated,
                        confirmed_cases_truncated)

# write_csv(output_results, path=file.path(outputDir, paste0("Simulated_deconvoluted_symptoms_v2.csv")))

ggplot(output_results_truncated) +
  geom_line(mapping = aes(x=date, y=value, colour=data_type), size = 1.5) + 
  coord_cartesian(ylim = c(0,2100)) +
  scale_colour_manual(name="", 
                      values=c(gg_color(3)[c(3,1)], "black"),
                      breaks = c( "Simulated Cases", 
                                  "Infections - deconvolved",
                                  "Infections - simulated"),
                      labels=c("Reports - simulated", "Infections - deconvolved", "Infections - simulated")) +
  scale_x_date(breaks="5 days", 
               date_labels = '%b\n%d',
               limits=c(as.Date("2020-04-20"), as.Date("2020-06-10"))) +
  ylab("Incidence") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = "right",
        legend.text = element_text(size=15),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12)) 

ggsave(filename = file.path(plotDir, "Deconvolution_simulated_symptoms_truncated_100_iterations.png"))
