#!/bin/sh
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

cd "../../../ch-hospital-data"
git pull

cd "$parent_path"
Rscript --vanilla --verbose 1_getRawData.R > 1_getRawData.Rout 2>&1
Rscript --vanilla --verbose 2_getInfectionIncidence.R > 2_getInfectionIncidence.Rout 2>&1
Rscript --vanilla --verbose 3_doReEstimates.R > 3_doReEstimates.Rout 2>&1
# summarize data in seperate process to avoid C stack limit
Rscript --vanilla --verbose 4_doReEstimatesSum.R > 4_doReEstimatesSum.Rout 2>&1
Rscript --vanilla --verbose 5_makeReffPlotly.R > 5_makeReffPlotly.Rout 2>&1

# crontab settings
# run every 3h starting at 1am
# 0 1,4,7,10,13,16,19,22 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
