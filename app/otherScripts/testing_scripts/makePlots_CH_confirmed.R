library("ggplot2")
library("lubridate")
library("readr")
library("gridExtra")
library(RColorBrewer)
library(reshape)
library(ggpubr)
library(wesanderson)
library("utils")
library("here")

### Help function for plotting
gg_color <- function(n) {
  
  hues = seq(15, 375, length = n + 1)
  
  hcl(h = hues, l = 65, c = 100)[1:n]
  
}

dataDir <- here::here("app/data/temp")

BAG_data_dir <- here::here("app", "data", "BAG")
BAG_data_dir_Git <- here::here("../ch-hospital-data/data/CH")

bagFiles <- c(
  # polybox
  list.files(BAG_data_dir,
             pattern = "*FOPH_COVID19_data_extract.csv",
             full.names = TRUE,
             recursive = TRUE),
  # git (legacy)
  list.files(BAG_data_dir_Git,
             pattern = "*FOPH_COVID19_data_extract.csv",
             full.names = TRUE,
             recursive = TRUE))

bagFileDates <- strptime(
  stringr::str_match(bagFiles, ".*\\/(\\d*-\\d*-\\d*_\\d*-\\d*-\\d*)")[, 2],
  format = "%Y-%m-%d_%H-%M-%S")

recent_dates <- unique(sort(bagFileDates, decreasing = T))[1:13]

i <- 1
# for(i in 1:length(recent_dates)) {
for(i in c(1,2)) {
  
  # estimates <- readRDS(file =  file.path(dataDir, paste0("Estimates_Re_raw_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  # estimates <- readRDS(file =  file.path(dataDir, paste0("Estimates_Re_raw_corrected_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  estimates <- readRDS(file =  file.path(dataDir, paste0("Estimates_Re_raw_corrected_rolling_average_", format(recent_dates[i], "%Y-%m-%d"), ".rds")))
  
  estimates_median <- estimates %>% 
    filter(region == "Switzerland",
           data_type == "Confirmed cases",
           estimate_type == "Cori_slidingWindow") %>% 
    group_by(variable, date) %>% 
    summarize(value = median(value)) %>%
    filter(date > as.Date("2020-03-03"))
  
  ggplot(estimates_median, aes(x = date)) +
    geom_line(aes(y = value, group = variable, linetype = !(variable == "R_mean"))) +
    scale_x_date(date_breaks = "4 days", 
                 date_labels = '%b\n%d') +
    coord_cartesian(ylim=c(0,5)) +
    geom_hline(yintercept = 1, linetype="dashed") + 
    xlab("") + 
    ylab("Reproductive number") +
    scale_linetype_discrete(labels=c("Mean", "Uncertainty interval")) +
    theme_bw() +
    theme(
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=10),
      axis.title.y =  element_text(size=15),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.position = "bottom")
  
  # ggsave(filename =  file.path(dataDir, paste0("Plot_Re_CH_", format(recent_dates[i], "%Y-%m-%d"), ".png")), width = 25, height = 10, units = "cm")
  # ggsave(filename =  file.path(dataDir, paste0("Plot_Re_CH_corrected", format(recent_dates[i], "%Y-%m-%d"), ".png")), width = 25, height = 10, units = "cm")
  ggsave(filename =  file.path(dataDir, paste0("Plot_Re_CH_corrected_rolling_average_", format(recent_dates[i], "%Y-%m-%d"), ".png")), width = 25, height = 10, units = "cm")
}

# for(i in 1:length(recent_dates)) {
#   
#   for(days_before in 1:4) {
#   
#   estimates <- readRDS(file =  file.path(dataDir, paste0("Estimates_Re_raw_", format(recent_dates[i], "%Y-%m-%d"), "_truncated_", days_before , "_day.rds")))
#   
#   estimates_median <- estimates %>% 
#     filter(region == "Switzerland",
#            data_type == "Confirmed cases",
#            estimate_type == "Cori_slidingWindow") %>% 
#     group_by(variable, date) %>% 
#     summarize(value = median(value)) %>%
#     filter(date > as.Date("2020-03-03"))
#   
#   ggplot(estimates_median, aes(x = date)) +
#     geom_line(aes(y = value, group = variable, linetype = !(variable == "R_mean"))) +
#     scale_x_date(date_breaks = "4 days", 
#                  date_labels = '%b\n%d') +
#     coord_cartesian(ylim=c(0,5)) +
#     geom_hline(yintercept = 1, linetype="dashed") + 
#     xlab("") + 
#     ylab("Reproductive number") +
#     scale_linetype_discrete(labels=c("Mean", "Uncertainty interval")) +
#     theme_bw() +
#     theme(
#       axis.text.y= element_text(size=14),
#       axis.text.x= element_text(size=10),
#       axis.title.y =  element_text(size=15),
#       legend.title = element_blank(),
#       legend.text = element_text(size=12),
#       legend.position = "bottom")
#   
#   ggsave(filename =  file.path(dataDir, paste0("Plot_Re_CH_", format(recent_dates[i], "%Y-%m-%d"), "_truncated_", days_before , "_day.png")),
#          width = 25, height = 10, units = "cm")
#   }
# }


