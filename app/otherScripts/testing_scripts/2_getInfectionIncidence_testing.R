cat(paste("###", Sys.time(), "- starting 2_getInfectionIncidence.R", "\n"))

library(lubridate)
library(readr)
library(utils)
library(tidyverse)
library(here)
library(fitdistrplus)
library(jcolors)


### Utilities ###
getLOESSCases <- function(dates, count_data, span=0.25){
  n_pad <- round(length(count_data)*span*0.5)
  c_data <- data.frame(value = c(rep(0, n_pad), count_data), 
                       date_num = c(seq(as.numeric(dates[1])-n_pad, as.numeric(dates[1])-1),
                                    as.numeric(dates)))
  c_data.lo <- loess(value ~ date_num, data = c_data, span = span)
  smoothed <- predict(c_data.lo)
  smoothed[smoothed < 0] = 0
  raw_smoothed_counts <- smoothed[(n_pad+1):length(smoothed)]
  normalized_smoothed_counts <- round(raw_smoothed_counts * sum(count_data, na.rm = T)/sum(raw_smoothed_counts, na.rm = T))
  return(normalized_smoothed_counts)
}


#### Build empirical CDF from draws summing samples from two gamma distributions
make_ecdf_from_gammas <- function(shape, scale, numberOfSamples = 1E6) {
  draws <-
    rgamma(numberOfSamples, shape = shape[1], scale = scale[1]) +
    rgamma(numberOfSamples, shape = shape[2], scale = scale[2])
  return(Vectorize(ecdf(draws)))
}

make_ecdf_from_empirical_data <- function(shape_incubation,
                                          scale_incubation,
                                          empirical_distr_onset_to_count){
  numberOfSamples <- length(empirical_distr_onset_to_count)*100
  draws <- rgamma(numberOfSamples, shape=shape_incubation[1], scale=scale_incubation[1]) + rep(empirical_distr_onset_to_count, times=100)
  return(Vectorize(ecdf(draws)))
}


get_dates_to_average_over <- function(i, dates, weeks_averaged) {
  if( i < (weeks_averaged * 7)) {
    return(dates[1:(weeks_averaged * 7)])
  } else {
    return(dates[(i - (weeks_averaged * 7)) : i])
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
  
  result_distributions <- list()
  
  if(!is_empirical) {
    #TODO implement checks that shape and scale have the right structure
    F_h <- make_ecdf_from_gammas(shape, scale)
  } else {
    all_delays <- (onset_to_count_empirical_delays %>% pull(delay))
    
    parms_gamma <- fitdist(all_delays + 1, "gamma")$estimate
    shape_gamma <- parms_gamma["shape"]
    rate_gamma <- parms_gamma["rate"]
    
    threshold_right_truncation <- ceiling(qgamma(upper_quantile_threshold, shape = shape_gamma, rate = rate_gamma) - 1)
  }
  
  for(i in 1:length(all_dates)) {
    
    if(is_empirical) {
      if(i > (length(all_dates) - threshold_right_truncation) & length(all_dates) > threshold_right_truncation ) {
        result_distributions[[i]] <- result_distributions[[length(all_dates) - threshold_right_truncation]]
        next
      }
      
      weeks_averaged <- 0
      repeat{
        weeks_averaged <- weeks_averaged + 1
        recent_counts_distribution <- onset_to_count_empirical_delays %>%
          filter( infection_date %in% get_dates_to_average_over(i, all_dates, weeks_averaged)) %>%
          pull( delay )
        
        if(length( recent_counts_distribution ) >= min_number_cases) {
          break
        }
      }
      
      F_h <- make_ecdf_from_empirical_data(shape,
                                           scale,
                                           recent_counts_distribution)
    }
    
    f <- Vectorize(function(x){
      if(x < 0) {
        return(0)
      } else if(x < 0.5) {
        return(F_h(0.5))
      } else {
        return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
      }
    })
    x <- 0:length_out
    result_distributions[[i]] <- f(x)
  }
  
  return( result_distributions )
}

prob_being_observed <- function(discretized_distr, currentIdx, lastObservedIdx) {
  
  p <- sum(sapply(currentIdx:lastObservedIdx, function(x) {
    discretized_distr[[currentIdx]][x + 1 - currentIdx]
  }))
  
  return(p)
}

get_original_extended_incidence <- function(original_data, max_delay = 20) {
  extension <- rep(0, times = max_delay)
  return(c(extension, original_data$value ))
}

get_chi_squared <- function(estimated_incidence, original_incidence, discretized_distr, max_delay){
  
  N <- length(original_incidence) - max_delay
  
  internal_sum <- sum(
    sapply(
      ( max_delay + 1 ) : length( original_incidence ), 
      function(index_i) {
        
        expected_reports_day_i <- get_expected_number_of_reports(estimated_incidence, discretized_distr, index_i)
        
        if( expected_reports_day_i == 0 ) {
          return( 0 )
        }
        return(( expected_reports_day_i - original_incidence[index_i])^2 / expected_reports_day_i)
      }
    )
  )
  
  return(internal_sum/N)
}

get_expected_number_of_reports <- function(estimated_incidence, discretized_distr, stop_idx) {
  return(
    sum(
      sapply(1:stop_idx, function(begin_idx) {
        return(discretized_distr[[begin_idx]][stop_idx - begin_idx  + 1] * estimated_incidence[begin_idx])
      }
      )
    )
  )
}

update_single_estimate_incidence_RL <- function(estimated_incidence, original_incidence, discretized_distr, index_j) {
  
  correcting_factor <- sum(
    sapply(index_j:length(estimated_incidence), function(index_i) {
      if(original_incidence[index_i] == 0) {
        return(0)
      }
      
      ## Can D_i == 0 happen and prob_being_observed is not zero?
      D_i <- get_expected_number_of_reports(estimated_incidence, discretized_distr, index_i)
      
      if(D_i == 0) {
        return(0)
      }
      
      result <- discretized_distr[[index_j]][index_i - index_j  +1] * original_incidence[index_i] / D_i
      return(result)
    })
  )
  
  q_j <- prob_being_observed(discretized_distr, index_j, length(estimated_incidence))
  
  if(q_j == 0) {
    return(0)
  }
  
  updated_value <- estimated_incidence[index_j] * correcting_factor / q_j
  return(updated_value)
} 

update_estimate_incidence_RL <- function(estimated_incidence, original_incidence, discretized_distr) {
  updated_estimate <- sapply(1:length(estimated_incidence), function(index_j){
    update_single_estimate_incidence_RL(estimated_incidence, original_incidence, discretized_distr, index_j)
  })
  return(updated_estimate)
}

iterate_RL <- function(initial_estimate, 
                       original_incidence, 
                       discretized_distr, 
                       threshold_chi_squared = 1, 
                       max_iterations = 20,
                       min_iterations = 3,
                       max_delay,
                       verbose = F){
  
  current_estimate <- initial_estimate
  
  count <- 1

  while( ((chi <- get_chi_squared(current_estimate, original_incidence, discretized_distr, max_delay = max_delay)) > threshold_chi_squared & count <= max_iterations) | count <= min_iterations ) {
    
    if( verbose ) {
      cat("\t\tStep: ", count, " - Chi squared: ", chi, "\n")
    }
    
    current_estimate <- update_estimate_incidence_RL(current_estimate, original_incidence, discretized_distr)
    count <- count + 1
  }
  
  return(current_estimate)
}

get_bootstrap_replicate <- function( original_time_series) {
  replicate <- original_time_series %>% 
    slice_sample(n = sum(original_time_series$value, na.rm =T),
                 weight_by = replace_na(value, 0),
                 replace = T) %>%
    group_by(country, region, source, data_type, variable, date) %>%
    mutate(value = n()) %>%
    distinct(date, .keep_all = T) %>%
    ungroup() %>% 
    group_by(country, region, source, data_type, variable) %>%
    complete( date = seq.Date( min(date), max(date), by = "days"),
              fill = list(value = 0)) %>%
    arrange(date)
  
  return(replicate) 
}

get_infection_incidence_by_deconvolution <- function(   data_subset,
                                                        shape_waiting_time,
                                                        scale_waiting_time,
                                                        min_chi_squared = 0.5,
                                                        maximum_iterations = 30,
                                                        minimum_iterations = 3,
                                                        max_first_guess_delay = 30,
                                                        absolute_max_reporting_delay = 100,
                                                        smooth_incidence = T,
                                                        empirical_delays  = tibble(),
                                                        numCasesWindow = 250,
                                                        n_bootstrap = 5,
                                                        verbose = F ) {
  
  length_initial_time_series <- data_subset %>% 
    dplyr::select(date) %>%
    complete( date = seq.Date( min(date),
                               max(date), 
                               by = "days" )) %>%
    pull() %>%
    length()
  
  minimal_date <- min(data_subset$date) - max_first_guess_delay
  maximal_date <- max(data_subset$date)
  
  all_dates <- seq(minimal_date, maximal_date, by = "days")
  
  infect_to_count_discretized_distr <- discretize_waiting_time_distr(is_empirical = (nrow(empirical_delays) > 0), 
                                                                     shape_waiting_time, 
                                                                     scale_waiting_time,
                                                                     onset_to_count_empirical_delays = empirical_delays,
                                                                     all_dates,
                                                                     length_out = length(all_dates) - max_first_guess_delay + absolute_max_reporting_delay)
  
  ## TODO this is temporary
  first_guess_delay <- which.max(infect_to_count_discretized_distr[[length(infect_to_count_discretized_distr)]])[1]
  
  if( verbose ) {
    cat("\tDelay on first guess: ", first_guess_delay, "\n")
  }
  
  results <- list(tibble())
  
  for(bootstrap_replicate_i in 0:n_bootstrap) {
    
    if(verbose == T) {
      cat("    Bootstrap replicate: ", bootstrap_replicate_i, "\n")
    }
    
    if(bootstrap_replicate_i == 0) {
      time_series <- data_subset
    } else {
      time_series <- get_bootstrap_replicate(data_subset)
    }
    
    if(smooth_incidence == T) {
      smoothed_incidence_data <- time_series %>%
        mutate( value = getLOESSCases( dates = date,
                                       count_data = value )) %>%
        complete( date = seq.Date( minimal_date,
                                   maximal_date, 
                                   by = "days" ),
                  fill = list(value = 0))
      
      raw_total_incidence <- sum(time_series$value, na.rm =T)
      smoothed_total_incidence <- sum(smoothed_incidence_data$value, na.rm = T)
      
      if(smoothed_total_incidence > 0) {
        smoothed_incidence_data <- smoothed_incidence_data %>%
          mutate( value = value * raw_total_incidence / smoothed_total_incidence)
      }
      
    } else {
      smoothed_incidence_data <- time_series  %>%
        complete( date = seq.Date( minimal_date,
                                   maximal_date, 
                                   by = "days" ),
                  fill = list(value = 0))
    }
    
    first_guess <- smoothed_incidence_data %>% 
      mutate(date = date - first_guess_delay) %>% 
      complete( date = seq.Date( minimal_date, maximal_date, by = "days" ),
                fill = list(value = 0)) %>%
      filter( date >=  minimal_date)
    
    final_estimate <- iterate_RL( first_guess$value, 
                                  smoothed_incidence_data$value, 
                                  discretized_distr = infect_to_count_discretized_distr, 
                                  threshold_chi_squared = min_chi_squared, 
                                  max_iterations = maximum_iterations,
                                  min_iterations = minimum_iterations,
                                  max_delay = first_guess_delay,
                                  verbose = verbose )
    
    ## right-truncate trailing zeroes induced by initial shift by 'first_guess_delay'
    deconvolved_dates <- first_guess %>%
      filter( date <= maximal_date - first_guess_delay ) %>%
      pull( date )
    
    deconvolved_infections <- final_estimate[ 1 : ( length(final_estimate) - first_guess_delay )]
    
    ## prepare metadata for result dataframe
    data_type_subset <- unique(time_series$data_type)[1]
    if(data_type_subset %in% c("Hospitalized patients - onset", "Hospitalized patients - admission")) {
      data_type_subset <- "Hospitalized patients"
    }
    data_type_name <- paste0("infection_", data_type_subset)
    
    ## dataframe containing results
    deconvolved_infections <- tibble(
      date = deconvolved_dates,
      region = unique(time_series$region)[1],
      country = unique(time_series$country)[1],
      source = unique(time_series$source)[1],
      data_type = data_type_name,
      replicate = bootstrap_replicate_i,
      value = deconvolved_infections,
      variable = "incidence"
    )
    
    results <- c(results, list(deconvolved_infections))
  }
  
  
  
  return( bind_rows(results) )
}

### One gamma-distributed waiting time is the incubation period
### The second one is the period between symptom onset and report (of positive test, death, hospitalization...)
get_all_infection_incidence <- function(  data,
                                          onset_to_count_empirical_delays = tibble(),
                                          data_types,
                                          shape_incubation,
                                          scale_incubation,
                                          shape_onset_to_count,
                                          scale_onset_to_count,
                                          min_chi_squared = 0.5,
                                          maximum_iterations = 30,
                                          minimum_iterations = 0,
                                          n_bootstrap = 5,
                                          verbose = F ){
  results <- list(tibble())
  
  is_delays_data_available <- (nrow(onset_to_count_empirical_delays) > 0)
  
  median_incubation_delay <- round(qgamma(0.5, shape = shape_incubation, scale = scale_incubation))
  
  for (count_type_i in data_types) {
    
    cat("Deconvolve infections for data type:", count_type_i, "\n")
    
    smooth <- (count_type_i != "Excess deaths")
    
    for (source_i in unique(data$source)) {
      
      cat("   Data source:", source_i, "\n")
      
      results_list <- lapply(
        unique(data$region),
        function(x) {
          cat("    Region:", x, "\n")
          subset_data <- data %>%
            filter(region == x,
                   source == source_i,
                   data_type == count_type_i,
                   variable == "incidence")
          
          if (nrow(subset_data) == 0) {
            return(tibble())
          }
          
          if(count_type_i == "Hospitalized patients") {
            data_types_included <- data %>%
              filter(region == x,
                     source == source_i,
                     variable == "incidence") %>%
              distinct(data_type) %>% 
              pull()
            
            if("Hospitalized patients - onset" %in% data_types_included) {
              return(tibble())
            }
          }
          
          if(is_delays_data_available) {
            empirical_delays <- onset_to_count_empirical_delays %>% 
              filter( region == x,
                      data_type == count_type_i) %>%
              mutate( infection_date = onset_date - median_incubation_delay)
          } else {
            empirical_delays <- tibble()
          }
          
          get_infection_incidence_by_deconvolution(   subset_data,
                                                      shape_waiting_time = c(shape_incubation, shape_onset_to_count[[count_type_i]]),
                                                      scale_waiting_time = c(scale_incubation, scale_onset_to_count[[count_type_i]]),
                                                      min_chi_squared,
                                                      maximum_iterations,
                                                      minimum_iterations,
                                                      smooth_incidence = smooth,
                                                      empirical_delays = empirical_delays,
                                                      n_bootstrap = n_bootstrap,
                                                      verbose = verbose)
          
        })
      results <- c(results, results_list)
    }
  }
  return(bind_rows(results))
}

#################################
#### Start of the script ########
#################################

####################
###### Input #######
####################

data_dir <- here::here("app/data/temp")
# raw_data_path <- file.path(data_dir, "Raw_data.rds")
# testing
# TODO comment out
raw_data_path <- file.path(data_dir, "Raw_data_testing_CH.rds")
# infect_data_path <- file.path(data_dir, "Deconvolved_infect_data.rds")
# infect_data_path <- file.path(data_dir, "Deconvolved_infect_rounding.rds")
infect_data_path <- file.path(data_dir, "Deconvolved_infect_prior.rds")

FOPH_data_path <- here::here("../ch-hospital-data/data/CH")
delays_data_path <- file.path(FOPH_data_path, "FOPH_data_delays.csv")

plots_save_path <- here::here("../../Plots/exploration_deconvolution")
delays_onset_to_count <- read_csv(delays_data_path,
                                  col_types = cols(
                                    data_type = col_character(),
                                    onset_date = col_date(format = ""),
                                    count_date = col_date(format = ""),
                                    delay = col_number()))


##TODO update
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
shape_onset_to_count <- c(mean_onset_to_count^2 / (sd_onset_to_count^2), "Hospitalized patients - onset" = 0)
scale_onset_to_count <- c((sd_onset_to_count^2) / mean_onset_to_count, "Hospitalized patients - onset" = 0)


###############

raw_data <- readRDS(file = raw_data_path)

## Look at bootstrap timeseries

# confirmed cases
# original_time_series <- raw_data %>%
#   filter(data_type == "Confirmed cases",
#                                             variable == "incidence",
#                                             region == "Switzerland",
#                                             source == "FOPH") %>%
#   mutate(replicate = 0)


## Look at bootstrapping and smoothing of bootstrap replicates

## commented out, already run
# investigate_time_series <- function(original_time_series, plot_path=plots_save_path, plot_name = "bootstrapping_exploration_CH_confirmed.png" ) {
#   bootstraps <- lapply(1:100, function(x) {
#     return(get_bootstrap_replicate(original_time_series) %>% mutate(replicate = x))
#   })
#   
#   raw_bootstraps_and_original <- bind_rows(original_time_series, bootstraps) %>%
#     mutate( raw = T)
#   
#   smoothed_bootstraps_and_original <- raw_bootstraps_and_original %>% 
#     group_by(replicate) %>%
#     mutate(value = replace_na(getLOESSCases(date, value), 0), raw = F) %>% ungroup()
#   
#   bootstraps_and_original <- bind_rows(raw_bootstraps_and_original, smoothed_bootstraps_and_original)
#   
#   ggplot() +
#     geom_line(data = filter(bootstraps_and_original, raw == F), aes(x = date, y = value, group = replicate, colour = replicate))
#   
#   ggplot() +
#     geom_point(data = filter(bootstraps_and_original, raw == T), aes(x = date, y = value, colour = replicate)) +
#     geom_line(data = filter(bootstraps_and_original, raw == F), aes(x = date, y = value,group = replicate, colour = replicate)) +
#     theme_light_bg() +
#     # scale_color_jcolors_contin("rainbow")
#     scale_color_jcolors_contin("pal3")
#   ggsave(filename = file.path(plot_path, plot_name), width = 20, height=15, units = "cm")
#   
#   
# }   
# 
# original_time_series <- raw_data %>%
#   filter(data_type == "Confirmed cases",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH") %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_confirmed.png")
# original_time_series <- raw_data %>%
#   filter(data_type == "Deaths",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH") %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_deaths.png")
# original_time_series <- raw_data %>%
#   filter(data_type == "Excess deaths",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "BFS") %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_excess_deaths.png")
# 
# original_time_series <- raw_data %>%
#   filter(data_type == "Excess deaths",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "BFS",
#          !is.na(value)) %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_excess_deaths.png")
# 
# original_time_series <- raw_data %>%
#   filter(data_type == "Hospitalized patients - admission",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH",
#          !is.na(value)) %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_hosp_admission.png")
# 
# original_time_series <- raw_data %>%
#   filter(data_type == "Hospitalized patients - onset",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH",
#          !is.na(value)) %>%
#   mutate(replicate = 0)
# investigate_time_series(original_time_series, plot_name = "bootstrapping_exploration_CH_hosp_onset.png")

## end of bootstrapping-focused part

## look at deconvolution of bootstrap replicates

# raw_data <- raw_data %>%
#   filter(data_type == "Confirmed cases",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH") %>%
#   mutate(replicate = 0)

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
  maximum_iterations = 30,
  n_bootstrap = 100,
  verbose = T)

##TESTING
# deconvolved_infections <- deconvolved_main_data

deconvolved_FOPH_hosp_data <- get_all_infection_incidence(
  raw_data,
  data_types = c("Hospitalized patients - admission", 
                "Hospitalized patients - onset"),
  shape_incubation = shape_incubation,
  scale_incubation = scale_incubation,
  shape_onset_to_count = shape_onset_to_count,
  scale_onset_to_count = scale_onset_to_count,
  min_chi_squared = 1,
  maximum_iterations = 30,
  n_bootstrap = 100,
  verbose = T)

## sum infections from Hospitalized patients - admission and Hospitalized patients - onset
deconvolved_FOPH_hosp_data <- deconvolved_FOPH_hosp_data %>%
  group_by(date, country, region, data_type, source, replicate, variable) %>%
  summarise(value=sum(value)) %>%
  arrange(country, region, source, data_type, variable, replicate, date) %>%
  ungroup()

deconvolved_infections <- bind_rows(deconvolved_main_data, deconvolved_FOPH_hosp_data) %>% ungroup()

saveRDS(deconvolved_infections, file = infect_data_path)


##PLOTTING

# ## look at bootstrap deconvolution
# deconvolved_infections <- readRDS(file = infect_data_path)
# 
# original_time_series <- raw_data %>%
#   filter(data_type == "Confirmed cases",
#          variable == "incidence",
#          region == "Switzerland",
#          source == "FOPH") %>%
#   mutate(replicate = 0) %>%
#   select(date, region, country, source, data_type, replicate, value, variable)
# 
# original_and_deconvolution <- bind_rows(original_time_series, deconvolved_infections)
# 
# ggplot(data = original_and_deconvolution, aes(x = date)) +
#   geom_line(data = filter(original_and_deconvolution, data_type == "infection_Confirmed cases"),
#             aes(x = date, 
#                 y = value, 
#                 group= replicate,
#                 colour = replicate)) +
#   geom_point( data = filter(original_and_deconvolution, data_type == "Confirmed cases"),
#                 aes(x = date, 
#                              y = value)) +
#   geom_line( data = filter(original_and_deconvolution, data_type == "Confirmed cases"),
#                       aes(x = date, 
#                                 y = value)) 
# ggsave(filename = file.path(plots_save_path, "deconvolved_bootstraps_week_average.png"), width = 20, height=15, units = "cm")

###############

cat(paste("###", Sys.time(), "- done 2_getInfectionIncidence.R", "\n"))
