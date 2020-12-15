#!/bin/sh
runRScript () {
  echo "running" $1 $2 "..."
  Rscript --verbose $1 $2 >>messagesCHE.Rout 2>>errorsCHE.Rout
  retVal=$?
  if [ $retVal -ne 0 ]; then
    echo "Script didn't run successfully (Error" $retVal ")"
  else
    echo "       " $1 $2 "done"
  fi
}

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

cd "$parent_path"

# deactivate crontab
cr=$(crontab -l)
if  [ ! -z "$cr" ]; then
  crontab -l > crontabBackup.txt
  echo "deactivating crontab. Backed up to crontabBackup.txt"
  crontab -r
fi

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

runRScript sumData.R
runRScript makeCHPlots.R

# reload data by restarting R shiny process
touch ../restart.txt

echo "updating covid19-Data ..."
cd "../../../dailyRe-Data"
git add .
git commit -m "update data"
git push

# update plots on eth cms
cd "../dailyRe/app/www/cantonPlots"
curl -nT "{$(echo *.png | tr ' ' ',')}" https://cms-author.ethz.ch/content/dam/ethz/special-interest/usys/ibz/theoreticalbiology/plots/

# reactivate crontab
if  [ ! -z "$cr" ]; then
  cd "$parent_path"
  echo "restoring crontab from backup"
  crontab crontabBackup.txt
fi
