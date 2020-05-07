#!/bin/sh
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

cd "../../../ch-hospital-data"
git pull

cd "$parent_path"
Rscript --vanilla --verbose 1_getRawData.R > 1_getRawData.Rout 2>&1
Rscript --vanilla --verbose 2_getInfectionIncidence.R > 2_getInfectionIncidence.Rout 2>&1
Rscript --vanilla --verbose 3_doReEstimates.R > 3_doReEstimates.Rout 2>&1
# format data in seperate process to avoid C stack limit
Rscript --vanilla --verbose 4_doReEstimatesFormat.R > 4_doReEstimatesFormat.Rout 2>&1
Rscript --vanilla --verbose 5_makeReffPlotly.R > 5_makeReffPlotly.Rout 2>&1

# crontab settings
# run everyday at 9am and 9pm
# 0 0,8,16 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
