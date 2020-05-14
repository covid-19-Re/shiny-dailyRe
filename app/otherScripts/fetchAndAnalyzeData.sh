#!/bin/sh
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

echo "updating ch-hospital-data ..."
cd "../../../ch-hospital-data"
git pull
echo "updating covid19-interventions ..."
cd "../covid19-additionalData"
git pull

cd "$parent_path"
rm *.Rout
# make temp data directory and clean last temp files
mkdir ../data/temp
rm -f ../data/temp/*
echo "running R scripts ..."
Rscript --vanilla --verbose 1_getRawData.R >> messages.Rout 2>> errors.Rout
Rscript --vanilla --verbose 2_getInfectionIncidence.R >> messages.Rout 2>> errors.Rout
Rscript --vanilla --verbose 3_doReEstimates.R >> messages.Rout 2>> errors.Rout
# summarize data in seperate process to avoid C stack limit
Rscript --vanilla --verbose 4_doReEstimatesSum.R >> messages.Rout 2>> errors.Rout
Rscript --vanilla --verbose 5_makeReffPlotly.R >> messages.Rout 2>> errors.Rout
# copy data
cp -f ../data/temp/* ../data/
# make app restart on next connection
touch ../restart.txt

# crontab settings
# run every 3h starting at 1am
# 0 1,4,7,10,13,16,19,22 * * * /home/covid-19-re/covid-19-re-shiny-app/app/otherScripts/fetchAndAnalyzeData.sh
