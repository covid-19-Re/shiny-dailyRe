#!/bin/sh

# deactivate crontab
crontab -l > /home/covid-19-re/crontabBackup.txt
crontab -r

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

runRScript () {
  echo "running" $1 $2 "..."
  Rscript --vanilla --verbose $1 $2 >>messagesCHE.Rout 2>>errorsCHE.Rout
  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "Script didn't run successfully (Error" $retVal ")"
  else
    echo "       " $1 $2 "done"
  fi
}

cd "$parent_path"

echo "updating covid19-additionalData ..."
cd "../../../covid19-additionalData"
git reset --hard HEAD
git pull

cd "$parent_path"
rm *CHE.Rout

echo "running R script to extract linelist data ..."
runRScript format_linelist_data.R

runRScript ReCountry.R "CHE"
runRScript ReCountry.R "LIE"
runRScript makeCHPlots.R

runRScript sumData.R

# reload data by restarting R shiny process
touch ../restart.txt

echo "updating covid19-Data ..."
cd "../../../dailyRe-Data"
git add .
git commit -m "update data"
git push

crontab /home/covid-19-re/dailyRe/app/otherScripts/crontab.txt
