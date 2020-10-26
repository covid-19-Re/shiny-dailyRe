cat(paste("###", Sys.time(), "- starting 2_getInfectionIncidence.R", "\n"))

library(lubridate)
library(readr)
library(utils)
library(tidyverse)
library(here)
library(fitdistrplus)

source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))
#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

data_dir <- here::here("app/data/exploration")
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
# incubation: mean = 5.3, sd =3.2 (Linton et al., best gamma distr fit)
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
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2), "Hospitalized patients - onset" = 0, "Symptoms" =0)
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count, "Hospitalized patients - onset" = 0, "Symptoms" =0)

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


##############




BAG_data_dir <- here::here("app", "data", "BAG")
### Find latest BAG data file

bagFiles <- c(
  list.files(BAG_data_dir,
             pattern = "*FOPH_COVID19_data_extract.csv",
             full.names = TRUE,
             recursive = TRUE))

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

recent_dates <- unique(sort(bagFileDates, decreasing = T))


i <- 1
# for(i in 1:length(recent_dates)) {
# for(i in c(1,2)) {
for(i in c(1)) {
  # raw_data <- readRDS(file = file.path(data_dir, paste0("Raw_data_CH", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  # raw_data <- readRDS(file = file.path(data_dir, paste0("Raw_data_CH_corrected_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  raw_data <- readRDS(file = file.path(data_dir, paste0("CH_data_with_correction_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  j <- 2
  
  # for(j in 0:8) {
  for(j in c(2)) {
    
    filtered_data <- raw_data %>% filter(date <= (max(date) -j) ) %>% filter(date > as.Date("2020-01-01"))
    deconvolved_main_data <- get_all_infection_incidence(
      filter(filtered_data, type == "raw"),
      onset_to_count_empirical_delays = delays_onset_to_count,
      constant_delay_distributions = constant_delay_distributions,
      data_types = c("Confirmed cases"),
      n_bootstrap = 10,
      verbose = T) %>%
      mutate(type = "raw")
    
    deconvolved_main_data_2 <- get_all_infection_incidence(
      filter(filtered_data, type == "corrected"),
      onset_to_count_empirical_delays = delays_onset_to_count,
      constant_delay_distributions = constant_delay_distributions,
      data_types = c("Confirmed cases"),
      n_bootstrap = 10,
      verbose = T)  %>%
      mutate(type = "corrected")
    
    # deconvolved_main_data_3 <- get_all_infection_incidence(
    #   filter(raw_data, type == "corrected_rolling"),
    #   onset_to_count_empirical_delays = delays_onset_to_count,
    #   data_types = c("Confirmed cases"),
    #   shape_incubation = shape_incubation,
    #   scale_incubation = scale_incubation,
    #   shape_onset_to_count = shape_onset_to_count,
    #   scale_onset_to_count = scale_onset_to_count,
    #   min_chi_squared = 1,
    #   maximum_iterations = 20,
    #   n_bootstrap = 50,
    #   verbose = T)  %>%
    #   mutate(type = "corrected_rolling")
    
    # deconvolved_infections <- deconvolved_main_data
    deconvolved_infections <- bind_rows(deconvolved_main_data, deconvolved_main_data_2) %>% ungroup()

  

smoothed_data <- filtered_data %>% 
  dplyr::group_by(region, country, source, data_type, variable, type) %>% 
  group_modify( ~ mutate(.x, value = getLOESSCases(dates = date, count_data = value))) %>% 
  ungroup() %>% 
  mutate(replicate = 0) %>% 
  mutate(data_type = "smoothed_Confirmed cases")
  
    all_data <- bind_rows(  mutate(filtered_data, replicate = 0), deconvolved_infections, smoothed_data) %>% ungroup()
    
    
    # ggplot(filter(all_data, data_type != "infection_Confirmed cases")) +
    print(ggplot(all_data) +
      geom_line(aes(x = date, y = value, color = interaction(data_type, type, replicate), group = interaction(replicate, data_type, type))) +
      scale_x_date(date_breaks = "3 days",
                   date_labels = '%b\n%d',
                   limits = c(as.Date("2020-05-15"), Sys.Date())) +
      xlab("") +
      theme(
        axis.text.y= element_text(size=14),
        axis.text.x= element_text(size=10),
        axis.title.y =  element_text(size=15),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position = "bottom") +
      coord_cartesian(ylim=c(0,170)) +
      ylab("Incidence") +
      theme_bw())
    
    # ggsave(file.path(data_dir, paste0("plot_ch_data_deconvolved_initial_fix_", format(recent_dates[i], "%Y-%m-%d"), "_truncation_", j ,".png")))
    # ggsave(file.path(data_dir, paste0("plot_ch_data_deconvolved_", format(recent_dates[i], "%Y-%m-%d"), "_truncation_", j ,".png")))
  
    # saveRDS(deconvolved_infections, file = file.path(data_dir, paste0("Deconvolved_infect_data_one_day_CH_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    # saveRDS(deconvolved_infections, file = file.path(data_dir, paste0("Deconvolved_infect_data_one_day_CH_corrected_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    # saveRDS(deconvolved_infections, file = file.path(data_dir, paste0("Deconvolved_infect_data_one_day_CH_corrected_rolling_average_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    # saveRDS(deconvolved_infections, file = file.path(data_dir, paste0("Deconvolved_infect_data_CH_corrected_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    # saveRDS(all_data, file = file.path(data_dir, paste0("Deconvolved_data_CH_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    # saveRDS(all_data, file = file.path(data_dir, paste0("Deconvolved_data_CH_initial_fix_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
    saveRDS(all_data, file = file.path(data_dir, paste0("Deconvolved_data_CH_initial_fix_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  }
}

i <- 1
for(i in c(1)) {
  
  all_data <- list(tibble())
  for(j in 0:8) {
   # data <- readRDS(file = file.path(data_dir, paste0("Deconvolved_data_CH_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
   data <- readRDS(file = file.path(data_dir, paste0("Deconvolved_data_CH_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
   # data <- readRDS(file = file.path(data_dir, paste0("Deconvolved_data_CH_initial_fix_truncated_",j, "_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
   all_data <- c(all_data, list(mutate(data, truncation = j)))
  }
  
  result <- bind_rows(all_data)
  
  
  print(
  ggplot(filter(result, data_type != "Confirmed cases", type == "corrected", replicate == 0)) +
    geom_line(aes(x = date, y = value, color = truncation, group = interaction(replicate, data_type, truncation))) +
    scale_x_date(date_breaks = "3 days",
                 date_labels = '%b\n%d',
                 limits = c(as.Date("2020-05-15"), Sys.Date())) +
    xlab("") +
    theme(
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=10),
      axis.title.y =  element_text(size=15),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.position = "bottom") +
    coord_cartesian(ylim=c(0,170)) +
    ylab("Incidence") +
    theme_bw())
  
  ggsave(file.path(data_dir, paste0("Smoothing_deconvolution_corrected_truncation_", format(recent_dates[i], "%Y-%m-%d") ,".png")))
  # ggsave(file.path(data_dir, paste0("Smoothing_deconvolution_initial_fix_corrected_truncation_", format(recent_dates[i], "%Y-%m-%d") ,".png")))
  
  print(
  ggplot(filter(result, data_type != "Confirmed cases", type == "raw", replicate == 0)) +
    geom_line(aes(x = date, y = value, color = truncation, group = interaction(replicate, data_type, truncation))) +
    scale_x_date(date_breaks = "3 days",
                 date_labels = '%b\n%d',
                 limits = c(as.Date("2020-05-15"), Sys.Date())) +
    xlab("") +
    theme(
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=10),
      axis.title.y =  element_text(size=15),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.position = "bottom") +
    coord_cartesian(ylim=c(0,170)) +
    ylab("Incidence") +
    theme_bw())
  
  ggsave(file.path(data_dir, paste0("Smoothing_deconvolution_raw_truncation_", format(recent_dates[i], "%Y-%m-%d") ,".png")))
  # ggsave(file.path(data_dir, paste0("Smoothing_deconvolution_initial_fix_raw_truncation_", format(recent_dates[i], "%Y-%m-%d") ,".png")))
  
}

# for(i in 1:length(recent_dates)) {
# 
#   for(days_before in 1:4) {
#     raw_data <- readRDS(file = file.path(data_dir, paste0("Raw_data_CH_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
# 
#     raw_data <- raw_data %>% filter(date <= (max(raw_data$date) - days_before))
# 
#     deconvolved_main_data <- get_all_infection_incidence(
#       raw_data,
#       onset_to_count_empirical_delays = delays_onset_to_count,
#       data_types = c("Confirmed cases"),
#       shape_incubation = shape_incubation,
#       scale_incubation = scale_incubation,
#       shape_onset_to_count = shape_onset_to_count,
#       scale_onset_to_count = scale_onset_to_count,
#       min_chi_squared = 1,
#       maximum_iterations = 20,
#       n_bootstrap = 50,
#       verbose = F)
# 
#     deconvolved_FOPH_hosp_data <- tibble()
# 
#     deconvolved_infections <- bind_rows(deconvolved_main_data, deconvolved_FOPH_hosp_data) %>% ungroup()
# 
# 
#     saveRDS(deconvolved_infections, file = file.path(data_dir, paste0("Deconvolved_infect_data_CH_", format(recent_dates[i], "%Y-%m-%d"), "_truncated_", days_before , "_day.rds")))
#   }
# }




###############

cat(paste("###", Sys.time(), "- done 2_getInfectionIncidence.R", "\n"))
