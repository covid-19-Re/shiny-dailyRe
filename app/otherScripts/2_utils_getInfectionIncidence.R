### Utilities ###
# smooth time series with LOESS method
getLOESSCases <- function(dates, count_data, days = 50) {
  span <- days / length(count_data)
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


get_vector_constant_waiting_time_distr <- function(shape_incubation,
                                                   scale_incubation,
                                                   shape_onset_to_report,
                                                   scale_onset_to_report,
                                                   length_out = 200,
                                                   n_random_samples = 1E6) {
  
  F_h <- make_ecdf_from_gammas(shape = c(shape_incubation, shape_onset_to_report), scale = c(scale_incubation, scale_onset_to_report))
  
  f <- Vectorize(function(x){
    if(x < 0) {
      return(0)
    } else if(x < 0.5) {
      return(F_h(0.5))
    } else {
      return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
    }
  })
  
  x <- 0:(length_out - 1)
  
  return(f(x))
}

get_matrix_constant_waiting_time_distr <- function(waiting_time_distr,
                                                   all_dates) {
  N <- length(all_dates)
  
  if(length(all_dates) >= length(waiting_time_distr)) {
    waiting_time_distr <- c(waiting_time_distr, rep(0, times = N - length(waiting_time_distr)))
  }
  
  delay_distribution_matrix <- matrix(0, nrow = N, ncol = N)
  for(i in 1:N) {
    delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), waiting_time_distr[1:(N - i + 1)])
  }
  
  return(delay_distribution_matrix)
}

get_matrix_empirical_waiting_time_distr <- function(shape_incubation,
                                                    scale_incubation,
                                                    onset_to_report_empirical_delays,
                                                    all_dates,
                                                    min_number_cases = 100,
                                                    upper_quantile_threshold = 0.99,
                                                    n_random_samples = 1E6){
  
  N <- length(all_dates)
  
  # take random samples from gamma distribution for incubation period
  gamma_draws <-  rgamma(n_random_samples, shape=shape_incubation, scale=scale_incubation)
  
  all_delays <- (onset_to_report_empirical_delays %>% pull(delay))
  
  F_h <- make_ecdf_from_empirical_data(gamma_draws,
                                       all_delays)
  
  f <- Vectorize(function(x){
    if(x < 0) {
      return(0)
    } else if(x < 0.5) {
      return(F_h(0.5))
    } else {
      return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
    }
  })
  
  x <- 0:N-1
  threshold_right_truncation <- which(cumsum(f(x)) > upper_quantile_threshold)[1] - 1
  
  delay_distribution_matrix <- matrix(0, nrow = N, ncol = N)
  
  # populate delay_distribution_matrix by column
  for(i in 1:N) {
    
    if(i > (N - threshold_right_truncation) & N > threshold_right_truncation ) {
      
      delay_distribution_matrix[, i ] <-  c(0, delay_distribution_matrix[1:(N-1), i - 1 ])
      next
    }
    
    weeks_averaged <- 0
    repeat{
      weeks_averaged <- weeks_averaged + 1
      recent_counts_distribution <- onset_to_report_empirical_delays %>%
        dplyr::filter( infection_date %in% get_dates_to_average_over(i, all_dates, weeks_averaged)) %>%
        pull( delay )
      
      if(length( recent_counts_distribution ) >= min_number_cases) {
        break
      }
    }
    
    F_h <- make_ecdf_from_empirical_data(gamma_draws,
                                         recent_counts_distribution)
    
    f <- Vectorize(function(x){
      if(x < 0) {
        return(0)
      } else if(x < 0.5) {
        return(F_h(0.5))
      } else {
        return(F_h(round(x + 1E-8) + 0.5) - F_h(round(x + 1E-8) - 0.5))
      }
    })
    
    last_index <- N - i
    x <- 0:last_index
    delay_distribution_matrix[, i ] <-  c(rep(0, times = i - 1 ), f(x))
  }
  
  return( delay_distribution_matrix )
}

iterate_RL <- function(
  initial_estimate,
  original_incidence,
  delay_distribution_matrix,
  Q_matrix,
  threshold_chi_squared = 1,
  max_iterations = 20,
  max_delay,
  verbose = FALSE) {
  
  current_estimate <- initial_estimate
  N <- length(current_estimate)
  chi_squared <- Inf
  count <- 1
  
  while(chi_squared > threshold_chi_squared & count <= max_iterations) {
    
    if (verbose) {
      cat("\t\tStep: ", count, " - Chi squared: ", chi_squared, "\n")
    }
    
    E <- as.vector(delay_distribution_matrix %*% current_estimate)
    
    N0 <- N - max_delay
    chi_squared <- 1/N0 * sum((E - original_incidence)^2/E, na.rm = T)
    
    B <- replace_na(original_incidence/E, 0)
    
    current_estimate <- current_estimate / Q_matrix *  as.vector(crossprod(B, delay_distribution_matrix))
    current_estimate <- replace_na(current_estimate, 0)
    count <- count + 1
  }
  
  return(current_estimate)
}

get_bootstrap_replicate <- function(original_time_series) {
  replicate <- original_time_series %>%
    dplyr::slice_sample(n = sum(original_time_series$value, na.rm = T),
                        weight_by = replace_na(value, 0),
                        replace = T) %>%
    dplyr::group_by(country, region, source, data_type, variable, date) %>%
    dplyr::mutate(value = n()) %>%
    distinct(date, .keep_all = T) %>%
    ungroup() %>%
    dplyr::group_by(country, region, source, data_type, variable) %>%
    complete(date = seq.Date(min(date), max(date), by = "days"),
             fill = list(value = 0)) %>%
    arrange(date)
  
  return(replicate)
}

get_infection_incidence_by_deconvolution <- function(
  data_subset,
  constant_delay_distribution,
  shape_incubation,
  scale_incubation,
  min_chi_squared = 1,
  maximum_iterations = 30,
  max_first_guess_delay = 30,
  smooth_incidence = T,
  empirical_delays  = tibble(),
  numCasesWindow = 250,
  n_bootstrap = 5,
  verbose = FALSE) {

  length_initial_time_series <- data_subset %>%
    dplyr::select(date) %>%
    complete(date = seq.Date(min(date), max(date), by = "days")) %>%
    pull() %>%
    length()

  minimal_date <- min(data_subset$date) - max_first_guess_delay
  maximal_date <- max(data_subset$date)

  all_dates <- seq(minimal_date, maximal_date, by = "days")

  is_empirical = (nrow(empirical_delays) > 0)

  if(is_empirical) {
    delay_distribution_matrix <- get_matrix_empirical_waiting_time_distr(
      shape_incubation,
      scale_incubation,
      empirical_delays,
      all_dates)
  } else {
    delay_distribution_matrix <- get_matrix_constant_waiting_time_distr(constant_delay_distribution, all_dates)
  }

  Q_matrix <- apply(delay_distribution_matrix, MARGIN = 2, sum)

  # use mode of 'constant_delay_distribution'. -1 because indices are offset by one as the delay can be 0.
  first_guess_delay <- which.max(constant_delay_distribution) - 1

  if (verbose) {
    cat("\tDelay on first guess: ", first_guess_delay, "\n")
  }

  results <- list(tibble())

  for (bootstrap_replicate_i in 0:n_bootstrap) {

    if (verbose == T) {
      cat("    Bootstrap replicate: ", bootstrap_replicate_i, "\n")
    }

    if (bootstrap_replicate_i == 0) {
      time_series <- data_subset
    } else {
      time_series <- get_bootstrap_replicate(data_subset)
    }

    if (smooth_incidence == T) {
      smoothed_incidence_data <- time_series %>%
        mutate(value = getLOESSCases(dates = date, count_data = value)) %>%
        complete(date = seq.Date(minimal_date, maximal_date, by = "days"), fill = list(value = 0))

      raw_total_incidence <- sum(time_series$value, na.rm = TRUE)
      smoothed_total_incidence <- sum(smoothed_incidence_data$value, na.rm = T)

      if (smoothed_total_incidence > 0) {
        smoothed_incidence_data <- smoothed_incidence_data %>%
          mutate(value = value * raw_total_incidence / smoothed_total_incidence)
      }

    } else {
      smoothed_incidence_data <- time_series  %>%
        complete(date = seq.Date(minimal_date, maximal_date, by = "days"), fill = list(value = 0))
    }

    ##TODO !!! change first guess !!!
    first_guess <- smoothed_incidence_data %>%
      mutate(date = date - first_guess_delay) %>%
      complete(date = seq.Date(minimal_date, maximal_date, by = "days"),
               fill = list(value = 0)) %>%
      filter(date >=  minimal_date)

    final_estimate <- iterate_RL(
      first_guess$value,
      smoothed_incidence_data$value,
      delay_distribution_matrix = delay_distribution_matrix,
      Q_matrix = Q_matrix,
      threshold_chi_squared = min_chi_squared,
      max_iterations = maximum_iterations,
      max_delay = first_guess_delay,
      verbose = verbose)

    ## right-truncate trailing zeroes induced by initial shift by 'first_guess_delay'
    deconvolved_dates <- first_guess %>%
      filter(date <= maximal_date - first_guess_delay) %>%
      pull(date)

    deconvolved_infections <- final_estimate[seq_len(length(final_estimate) - first_guess_delay)]

    ## prepare metadata for result dataframe
    data_type_subset <- unique(time_series$data_type)[1]
    if (data_type_subset %in% c("Hospitalized patients - onset", "Hospitalized patients - admission")) {
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

  return(bind_rows(results))
}


### One gamma-distributed waiting time is the incubation period
### The second one is the period between symptom onset and report (of positive test, death, hospitalization...)
get_all_infection_incidence <- function(data,
                                        constant_delay_distributions,
                                        onset_to_count_empirical_delays = tibble(),
                                        data_types,
                                        shape_incubation,
                                        scale_incubation,
                                        min_chi_squared = 0.5,
                                        maximum_iterations = 30,
                                        n_bootstrap = 5,
                                        verbose = FALSE) {
  results <- list(tibble())
  
  is_delays_data_available <- (nrow(onset_to_count_empirical_delays) > 0)
  
  median_incubation_delay <- round(qgamma(0.5, shape = shape_incubation, scale = scale_incubation))
  
  for (count_type_i in data_types) {
    
    cat("Deconvolve infections for data type:", count_type_i, "\n")
    
    smooth <- (count_type_i != "Excess deaths")
    
    for (source_i in unique(data$source)) {
      
      cat("   Data source:", source_i, "\n")
      # nCores <- max(1, parallel::detectCores() - 1)
      # cat("   calculating on", nCores, "cores...\n")
      # cl <- parallel::makeCluster(nCores, type = "FORK", outfile = "")
      thisData <- data %>%
        filter(source == source_i, variable == "incidence")
      results_list <- lapply(# parallel::parLapply(cl,
        unique(thisData$region),
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
          
          if (count_type_i == "Hospitalized patients") {
            data_types_included <- data %>%
              filter(region == x,
                     source == source_i,
                     variable == "incidence") %>%
              distinct(data_type) %>%
              pull()
            
            if ("Hospitalized patients - onset" %in% data_types_included) {
              return(tibble())
            }
          }
          
          if (is_delays_data_available) {
            empirical_delays <- onset_to_count_empirical_delays %>%
              filter(
                region == x,
                data_type == count_type_i) %>%
              mutate(
                infection_date = onset_date - median_incubation_delay)
          } else {
            empirical_delays <- tibble()
          }
          
          get_infection_incidence_by_deconvolution(
            subset_data,
            constant_delay_distribution = constant_delay_distributions[[count_type_i]],
            shape_incubation = shape_incubation,
            scale_incubation = scale_incubation,
            min_chi_squared,
            maximum_iterations,
            smooth_incidence = smooth,
            empirical_delays = empirical_delays,
            n_bootstrap = n_bootstrap,
            verbose = verbose)
        })
      #parallel::stopCluster(cl)
      results <- c(results, results_list)
    }
  }
  return(bind_rows(results))
}
