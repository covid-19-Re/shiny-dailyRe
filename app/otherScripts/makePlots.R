library("ggplot2")
library("lubridate")
library("readr")
library("gridExtra")
library("reshape2")
library(RColorBrewer)
library(reshape)
library(ggpubr)
library(wesanderson)
library("utils")

### Help function for plotting
gg_color <- function(n) {
  
  hues = seq(15, 375, length = n + 1)
  
  hcl(h = hues, l = 65, c = 100)[1:n]
  
}


makePanelRawData <- function(data, cantonList=c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH"), startDateRawData=as.Date("2020-02-25")){
  
  #### Plot incidence and cumulative cases
  data <- subset(data, !(region!="CH" & data_type=="deaths"))
  castRawData <- cast(data)
  castRawData$region <- factor(castRawData$region, levels=cantonList)
  
  pCases <- ggplot(castRawData, aes(x=date)) +
    facet_grid(region ~.) +
    geom_line(aes(y = cumul, group=data_type, color=data_type), size=1) + 
    geom_bar(aes(y = incidence, fill=data_type), stat = "identity", position=position_dodge(preserve="single"), width=1,alpha=1) + 
    scale_x_date(date_breaks = "6 days", 
                 date_labels = '%b\n%d',
                 limits = c(startDateRawData, Sys.Date()-1)) +
    scale_y_log10() + 
    ylab("Cumulative (line) and daily (bars) numbers") + 
    xlab("") +
    scale_colour_manual(values=c("black", gg_color(3)[c(1,3)]),
                        labels=c("Confirmed cases", "Hospitalizations", "Deaths"),
                        breaks=c("confirmed", "hospitalized", "deaths"),
                        name  ="Data source", 
                        aesthetics = c("colour", "fill")) +
    theme_bw() + 
    theme(
      strip.background = element_blank(),
      strip.text.y = element_blank(),
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=14),
      axis.title.y =  element_text(size=17),
      legend.title = element_text(size=17),
      legend.text = element_text(size=15)
    )
  
  return(pCases)
}

makePanelEstimates <- function(castEstimates, estimateType, lastDayBAGData=Sys.Date(),  startDate=as.Date("2020-03-07"), endDate=(Sys.Date() - 11)) {
  
  ## panel for Re estimates with sliding window
  pRe <- ggplot(subset(castEstimates, estimate_type==estimateType), aes(x=date)) +
    facet_grid(region ~.) +
    geom_ribbon(aes(ymin=median_R_lowHPD,ymax=median_R_highHPD, fill=data_type),alpha=0.7, colour=NA) +
    geom_ribbon(aes(ymin=lowQuantile_R_lowHPD,ymax=highQuantile_R_highHPD, fill=data_type),alpha=0.15, colour=NA) +
    geom_line(aes(y = median_R_mean, group=data_type, color=data_type), size=1.1) +
    geom_hline(yintercept = 1, linetype="dashed") + 
    scale_x_date(date_breaks = "4 days", 
                 date_labels = '%b\n%d',
                 limits = c(startDate, endDate)) + 
    coord_cartesian(ylim=c(0,3)) +
    geom_vline(xintercept = c(as.Date("2020-03-14"), as.Date("2020-03-17"), as.Date("2020-03-20")), linetype="dotted") + 
    geom_vline(xintercept = c(as.Date("2020-04-13")), linetype="dashed") + 
    annotate("rect", xmin=as.Date("2020-03-14"), xmax=as.Date("2020-03-17"), ymin=-1, ymax=Inf, alpha=0.45, fill="grey") +
    scale_colour_manual(values=c(gg_color(3)[c(1,3)], "black"),
                        labels=c("Confirmed cases",  "Deaths", "Hospitalized"),
                        breaks=c("infection_confirmed", "infection_deaths", "infection_hospitalized"),
                        name  ="Data source",
                        aesthetics = c("fill", "color")) +
    xlab("") + 
    ylab("Reproductive number") + 
    theme_bw() + 
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size=16),
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=14),
      axis.title.y =  element_text(size=17),
      legend.title = element_text(size=14),
      legend.text = element_text(size=12)
    )
  
  return(pRe)
}

makeWebsitePlot <- function(data, castEstimates, cantonList=c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH"), lastDayBAGData=Sys.Date(), outputDir) {
  
  pCases <- makePanelRawData(data, cantonList)
  pRe_window <- makePanelEstimates(castEstimates, "Cori_slidingWindow", lastDayBAGData=Sys.Date(),  startDate=as.Date("2020-03-07"), endDate=(Sys.Date() - 11)) 
  pRe_step <- makePanelEstimates(castEstimates, "Cori_step", lastDayBAGData=Sys.Date(),  startDate=as.Date("2020-03-07"), endDate=(Sys.Date() - 11)) 
  ## make full plot
  ggarrange(pCases,pRe_window,pRe_step,
            nrow = 1, 
            labels = c("A", "B", "C"),
            font.label = list(size = 18),
            common.legend = TRUE,
            legend = "bottom")
  
  plotPath <- file.path(outputDir, paste0("Re_CH_", gsub("-","", Sys.Date()), ".png"))
  ggsave(plotPath, width = 40, height = 60, units = "cm")
  plotPathCommon <- plotPath <- file.path(outputDir, paste0("Re_CH.png"))
  ggsave(plotPathCommon, width = 40, height = 60, units = "cm")
}

castEstimateData <- function(estimates, cantonList=c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH")){
  
  estimates$replicate <- as.factor(estimates$replicate)
  estimates <- subset(estimates, !(region != "CH" & data_type == "infection_deaths"))
  castEstimates <- cast(estimates)
  
  castEstimates <- subset(castEstimates, !(data_type == "infection_deaths" & date > (Sys.Date() - 16) ) & !(data_type == "infection_hospitalized" & date > lastDayBAGData))
  
  castEstimates$data_type <- factor(castEstimates$data_type, levels=c("infection_deaths", "infection_hospitalized","infection_confirmed"))
  castEstimates$region <- factor(castEstimates$region, levels=cantonList)
  
  ## Compute median and uncertainty intevals, grouping by everything but replicates
  castEstimates$median_R_mean <- with(castEstimates, ave(R_mean, date, region, data_type, source, estimate_type, FUN = median ))
  castEstimates$median_R_highHPD <- with(castEstimates, ave(R_highHPD, date, region, data_type, source, estimate_type, FUN = median ))
  castEstimates$median_R_lowHPD <- with(castEstimates, ave(R_lowHPD, date, region, data_type, source, estimate_type, FUN = median ))
  
  castEstimates$highQuantile_R_highHPD <- with(castEstimates, ave(R_highHPD, date, region, data_type, source, estimate_type, FUN = function(x) quantile(x, probs=0.975, na.rm=T) ))
  castEstimates$lowQuantile_R_lowHPD <- with(castEstimates, ave(R_lowHPD, date, region, data_type, source, estimate_type, FUN = function(x) quantile(x, probs=0.025, na.rm=T) ))
  
  return(castEstimates)
}

saveEstimatesAsCSV <- function(castEstimates, outputDir, startDate=as.Date("2020-03-07"), endDate=(Sys.Date() - 11)){
  #### Save estimates in CSV files
  dirPath_step <- file.path(outputDir, paste0(Sys.Date(), "_step"))
  dirPath_window <- file.path(outputDir, paste0(Sys.Date(), "_slidingWindow"))
  dir.create(dirPath_step, showWarnings = FALSE)
  dir.create(dirPath_window, showWarnings = FALSE)
  
  for (reg in unique(castEstimates$region)) {
    
    estimates_confirmed <- subset(castEstimates, data_type %in% c("infection_confirmed") & date >= startDate & date <= endDate & region==reg)
    estimates_confirmed <- subset(estimates_confirmed, replicate == 1)
    estimates_step <- subset(estimates_confirmed, estimate_type=="Cori_step")
    estimates_window <- subset(estimates_confirmed, estimate_type=="Cori_slidingWindow")
    
    drop_cols <- c("estimate_type", "data_type", "region", "replicate", "cumul", "incidence", "R_highHPD", "R_lowHPD", "highQuantile_R_highHPD", "lowQuantile_R_lowHPD",  "R_mean", "estimated")
    
    data_to_save_step <- estimates_step[, !names(estimates_step) %in% drop_cols]
    data_to_save_window <- estimates_window[, !names(estimates_window) %in% drop_cols]
    
    colnames(data_to_save_step) <- c("date", "R_mean", "R_highCI", "R_lowCI")
    colnames(data_to_save_window) <- c("date", "R_mean", "R_highCI", "R_lowCI")
    
    write_excel_csv(format(data_to_save_step, digits=3), path = file.path(dirPath_step, paste0(reg, "_R_estimates.csv")), quote=F)
    write_excel_csv(format(data_to_save_window, digits=3), path = file.path(dirPath_window, paste0(reg, "_R_estimates.csv")), quote=F)
  }
  
  ## Create zip archive from each estimate type (to be uploaded to webpage)
  csvFiles <- dir(dirPath_step, full.names = TRUE)
  zipFile_step <- file.path(outputDir, "Re_CH_step.zip")
  zip(zipfile = zipFile_step, flags="-r9Xj", files = csvFiles) # flag "-j" added  to default flags to not save entire dir tree
  
  csvFiles <- dir(dirPath_window, full.names = TRUE)
  zipFile_window <- file.path(outputDir, "Re_CH_slidingWindow.zip")
  zip(zipfile = zipFile_window, flags="-r9Xj", files = csvFiles) # flag "-j" added to default flags to not save entire dir tree
}

### Make plot and csv files for "https://bsse.ethz.ch/cevo/research/sars-cov-2/real-time-monitoring-in-switzerland.html"
makeWebsitePlotAndFiles <- function(data, estimates, cantonList=c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH"), lastDayBAGData,  startDateEstimates=as.Date("2020-03-07"), endDate=(Sys.Date() - 11), outputDir){
  

  
  ## panel with piecewise constant Re estimates
  pRe_step <- ggplot(subset(estimates, estimate_type=="Cori_step"), aes(x=date)) +
    facet_grid(region ~.) +
    geom_ribbon(aes(ymin=median_R_lowHPD,ymax=median_R_highHPD, fill=data_type),alpha=0.7, colour=NA) +
    geom_ribbon(aes(ymin=lowQuantile_R_lowHPD,ymax=highQuantile_R_highHPD, fill=data_type),alpha=0.15, colour=NA) +
    geom_line(aes(y = median_R_mean, group=data_type, color=data_type), size=1.1) +
    geom_hline(yintercept = 1, linetype="dashed") + 
    scale_x_date(date_breaks = "4 days", 
                 date_labels = '%b\n%d',
                 limits = c(startDate, endDate)) + 
    coord_cartesian(ylim=c(0,3)) +
    geom_vline(xintercept = c(as.Date("2020-03-14"), as.Date("2020-03-17"), as.Date("2020-03-20")), linetype="dotted") + 
    geom_vline(xintercept = c(as.Date("2020-04-13")), linetype="dashed") + 
    annotate("rect", xmin=as.Date("2020-03-14"), xmax=as.Date("2020-03-17"), ymin=-1, ymax=Inf, alpha=0.45, fill="grey") +
    scale_colour_manual(values=c(gg_color(3)[c(1,3)], "black"),
                        labels=c("Confirmed cases",  "Deaths", "Hospitalized"),
                        breaks=c("infection_confirmed", "infection_deaths", "infection_hospitalized"),
                        name  ="Data source",
                        aesthetics = c("fill", "color")) +
    xlab("") + 
    ylab("Reproductive number") + 
    theme_bw() + 
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size=16),
      axis.text.y= element_text(size=14),
      axis.text.x= element_text(size=14),
      axis.title.y =  element_text(size=17),
      legend.title = element_text(size=14),
      legend.text = element_text(size=12)
    )
  
  ## make full plot
  ggarrange(pCases,pRe_window,pRe_step,
            nrow = 1, 
            labels = c("A", "B", "C"),
            font.label = list(size = 18),
            common.legend = TRUE,
            legend = "bottom")
  
  plotPath <- file.path(outputDir, paste0("Re_CH_", gsub("-","", Sys.Date()), ".png"))
  ggsave(plotPath, width = 40, height = 60, units = "cm")
  plotPathCommon <- plotPath <- file.path(outputDir, paste0("Re_CH.png"))
  ggsave(plotPathCommon, width = 40, height = 60, units = "cm")
  
  
  #### Save estimates in CSV files
  regionsToSave <- cantonList
  
  dirPath_step <- file.path(outputDir, paste0(Sys.Date(), "_step"))
  dirPath_window <- file.path(outputDir, paste0(Sys.Date(), "_slidingWindow"))
  dir.create(dirPath_step, showWarnings = FALSE)
  dir.create(dirPath_window, showWarnings = FALSE)
  
  for (reg in regionsToSave) {
    
    estimates_confirmed <- subset(castData, estimated==T & estimate_type %in% c("Cori_slidingWindow", "Cori_step") & data_type %in% c("infection_confirmed") & date >= startDate & date <= endDate & region==reg)
    estimates_confirmed$median_R_mean <- with(estimates_confirmed, ave(R_mean, date, region, data_type, estimate_type, FUN = median ))
    estimates_confirmed$median_R_highHPD <- with(estimates_confirmed, ave(R_highHPD, date, region, data_type, estimate_type, FUN = median ))
    estimates_confirmed$median_R_lowHPD <- with(estimates_confirmed, ave(R_lowHPD, date, region, data_type, estimate_type, FUN = median ))
    
    estimates_confirmed <- subset(estimates_confirmed, replicate == 1)
    estimates_step <- subset(estimates_confirmed, estimate_type=="Cori_step")
    estimates_window <- subset(estimates_confirmed, estimate_type=="Cori_slidingWindow")
    
    drop_cols <- c("estimate_type", "data_type", "region", "replicate", "cumul", "incidence", "R_highHPD", "R_lowHPD", "R_mean", "estimated")
    
    data_to_save_step <- estimates_step[, !names(estimates_step) %in% drop_cols]
    data_to_save_window <- estimates_window[, !names(estimates_window) %in% drop_cols]
    
    colnames(data_to_save_step) <- c("date", "R_mean", "R_highCI", "R_lowCI")
    colnames(data_to_save_window) <- c("date", "R_mean", "R_highCI", "R_lowCI")
    
    write_excel_csv(format(data_to_save_step, digits=3), path = file.path(dirPath_step, paste0(reg, "_R_estimates.csv")), quote=F)
    write_excel_csv(format(data_to_save_window, digits=3), path = file.path(dirPath_window, paste0(reg, "_R_estimates.csv")), quote=F)
  }
  
  ## Create zip archive from each estimate type (to be uploaded to webpage)
  csvFiles <- dir(dirPath_step, full.names = TRUE)
  zipFile_step <- file.path(outputDir, "Re_CH_step.zip")
  zip(zipfile = zipFile_step, flags="-r9Xj", files = csvFiles) # flag "-j" added  to default flags to not save entire dir tree
  
  csvFiles <- dir(dirPath_window, full.names = TRUE)
  zipFile_window <- file.path(outputDir, "Re_CH_slidingWindow.zip")
  zip(zipfile = zipFile_window, flags="-r9Xj", files = csvFiles) # flag "-j" added to default flags to not save entire dir tree
  
}


#################################
#### Start of the script ########
#################################


####################
###### Input #######
####################

outputDir <- here("app/data")
pathToEstimatesReSave <- file.path(outputDir, paste0("Estimates_Re_",Sys.Date(), ".Rdata"))
pathToRawDataSave <- file.path(outputDir, paste0("Raw_data_",Sys.Date(), ".Rdata"))

lastDayBAGData <- as.Date("2020-04-28")
orderedListOfRegions <- c("AG", "BE", "BL", "BS", "FR", "GE", "GR", "LU", "NE", "SG", "TI", "VD", "VS", "ZH", "CH")


#### data
load(file=pathToRawDataSave)
load(file=pathToEstimatesReSave)

## cast estimates from long format
castEstimates <- castEstimateData(estimatesRe, orderedListOfRegions)

## make plot
makeWebsitePlot(rawData, castEstimates, cantonList=orderedListOfRegions, lastDayBAGData = lastDayBAGData, outputDir=outputDir)

## save in csv files
saveEstimatesAsCSV(castEstimates, outputDir)
