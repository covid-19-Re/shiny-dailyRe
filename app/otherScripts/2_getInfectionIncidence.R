cat(paste("###", Sys.time(), "- starting 2_getInfectionIncidence.R", "\n"))

library(lubridate)
library(readr)
library(utils)
library(tidyverse)
library(here)
library(fitdistrplus)
library(parallel)
library(Matrix)

# source(here::here("app/otherScripts/2_utils_getInfectionIncidence.R"))

getLOESSCases <- function(dates, count_data, span = 0.25) {
  n_pad <- round(length(count_data) * span * 0.5)
  c_data <- data.frame(value = c(rep(0, n_pad), count_data),
                       date_num = c(seq(as.numeric(dates[1]) - n_pad, as.numeric(dates[1]) - 1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = span)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] <- 0
  raw_smoothed_counts <- smoothed[(n_pad + 1):length(smoothed)]
  normalized_smoothed_counts <- round(
    raw_smoothed_counts * sum(count_data, na.rm = T) / sum(raw_smoothed_counts, na.rm = T))
  return(normalized_smoothed_counts)
}

#### Build empirical CDF from draws summing samples from two gamma distributions
make_ecdf_from_gammas <- function(shape, scale, numberOfSamples = 1E6) {
  draws <-
    rgamma(numberOfSamples, shape = shape[1], scale = scale[1]) +
    rgamma(numberOfSamples, shape = shape[2], scale = scale[2])
  return(Vectorize(ecdf(draws)))
}

make_ecdf_from_empirical_data <- function(gamma_draws,
                                          empirical_distr_onset_to_count){
  
  multiplier <- 100
  
  while(length(gamma_draws) < (length(empirical_distr_onset_to_count)*multiplier) & multiplier > 1) {
    multiplier <- floor(multiplier * 0.8)
  }
  
  if(multiplier == 1) {
    final_length <- min(length(gamma_draws), length(empirical_distr_onset_to_count))
    
    draws <- sample(gamma_draws, final_length, replace = F) + sample(empirical_distr_onset_to_count, final_length, replace = F)
  } else {
    draws <- gamma_draws[1:(length(empirical_distr_onset_to_count)*multiplier)] + rep(empirical_distr_onset_to_count, times=multiplier)
  }
  
  return(ecdf(draws))
}



get_dates_to_average_over <- function(i, dates, weeks_averaged) {
  if (i < (weeks_averaged * 7)) {
    return(dates[seq_len(weeks_averaged * 7)])
  } else {
    return(dates[(i - (weeks_averaged * 7)):i])
  }
}

discretize_waiting_time_distr <- function(is_empirical = F,
                                          shape,
                                          scale,
                                          onset_to_count_empirical_delays = tibble(),
                                          all_dates,
                                          min_number_cases = 100,
                                          length_out = 250,
                                          upper_quantile_threshold = 0.99){
  
  N <- length(all_dates)
  delay_distribution_matrix <- matrix(0, nrow = N, ncol = N)
  
  if(!is_empirical) {
    #TODO implement checks that shape and scale have the right structure
    F_h <- make_ecdf_from_gammas(shape, scale)
    
    f <- Vectorize(function(x){
      if(x < 0) {
        return(0)
      } else if(x < 0.5) {
        return(F_h(0.5))
      } else {
        return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
      }
    })
    
    for(i in 1:N) {
      last_index <- N - i
      x <- 0:last_index
      delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), f(x))
    }
    
  } else {
    
    f <- Vectorize(function(x){
      if(x < 0) {
        return(0)
      } else if(x < 0.5) {
        return(F_h(0.5))
      } else {
        return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
      }
    })
    
    all_delays <- (onset_to_count_empirical_delays %>% pull(delay))
    
    parms_gamma <- fitdist(all_delays + 1, "gamma")$estimate
    shape_gamma <- parms_gamma["shape"]
    rate_gamma <- parms_gamma["rate"]
    
    threshold_right_truncation <- ceiling(qgamma(upper_quantile_threshold, shape = shape_gamma, rate = rate_gamma) - 1)
    
    numberOfSamples <- 1E6
    gamma_draws <-  rgamma(numberOfSamples, shape=shape[1], scale=scale[1])
    
    for(i in 1:N) {
      
      if(i > (N - threshold_right_truncation) & N > threshold_right_truncation ) {
        
        delay_distribution_matrix[, i ] <-  c(0, delay_distribution_matrix[1:(N-1), i - 1 ])
        next
      }
      
      weeks_averaged <- 0
      repeat{
        weeks_averaged <- weeks_averaged + 1
        recent_counts_distribution <- onset_to_count_empirical_delays %>%
          dplyr::filter( infection_date %in% get_dates_to_average_over(i, all_dates, weeks_averaged)) %>%
          pull( delay )
        
        if(length( recent_counts_distribution ) >= min_number_cases) {
          break
        }
      }
      
      F_h <- make_ecdf_from_empirical_data(gamma_draws,
                                           recent_counts_distribution)
      
      last_index <- N - i
      x <- 0:last_index
      delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), f(x))
    }
  }
  
  return( delay_distribution_matrix )
}


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

### gamma distribution parameters for incubation period
shape_incubation <- mean_incubation^2 / (sd_incubation^2)
scale_incubation <- (sd_incubation^2) / mean_incubation

mean_onset_to_count <- c(
  "Confirmed cases" = mean_onset_to_test)
sd_onset_to_count <- c(
  "Confirmed cases" = sd_onset_to_test)

### parameters for gamma distribution between symptom onset and report
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2))
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count)


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

max_first_guess_delay <- 30
minimal_date <- min(raw_data$date) - max_first_guess_delay
maximal_date <- max(raw_data$date)

x <- "Switzerland"
count_type_i <- "Confirmed cases"
median_incubation_delay <- round(qgamma(0.5, shape = shape_incubation, scale = scale_incubation))

all_dates <-  seq.Date(minimal_date, maximal_date, by = "days")

is_empirical = F
shape = shape_incubation
scale = scale_incubation
empirical_delays = delays_onset_to_count %>%
  filter(region == x,
         data_type == count_type_i) %>%
  mutate(
    infection_date = onset_date - median_incubation_delay)
min_number_cases = 100
length_out = 250
upper_quantile_threshold = 0.99
shape_waiting_time <- shape_incubation
scale_waiting_time <- scale_incubation


data_subset <- raw_data %>% filter(region == x, data_type == count_type_i)
# get_infection_incidence_by_deconvolution
length_initial_time_series <- data_subset %>%
  dplyr::select(date) %>%
  complete(date = seq.Date(min(date), max(date), by = "days")) %>%
  pull() %>%
  length()

minimal_date <- min(data_subset$date) - max_first_guess_delay
maximal_date <- max(data_subset$date)

all_dates <- seq(minimal_date, maximal_date, by = "days")


## calculations
infect_to_count_discretized_distr <- discretize_waiting_time_distr(
  is_empirical = (nrow(empirical_delays) > 0),
  shape_waiting_time,
  scale_waiting_time,
  onset_to_count_empirical_delays = empirical_delays,
  all_dates,
  length_out = length(all_dates) - max_first_guess_delay + absolute_max_reporting_delay)


N <- nrow(infect_to_count_discretized_distr)
# take the mode halfway in the time series
first_guess_delay <- 8



