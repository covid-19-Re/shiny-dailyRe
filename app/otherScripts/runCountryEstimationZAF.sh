#!/bin/sh

# deactivate crontab
cr=$(crontab -l)
if  [ ! -z "$cr" ]; then
  crontab -l > crontabBackup.txt
  echo "deactivating crontab. Backed up to crontabBackup.txt"
  crontab -r
fi

parent_path=$(
  cd "$(dirname "${BASH_SOURCE[0]}")"
  pwd -P
)

runRScript () {
  echo "running" $1 $2 "..."
  Rscript --verbose $1 $2 >>messagesZAF.Rout 2>>errorsZAF.Rout
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
rm *ZAF.Rout

echo "running R script to extract linelist data ..."
runRScript format_linelist_data.R

runRScript ReCountry.R "ZAF"

runRScript sumData.R

# reload data by restarting R shiny process
touch ../restart.txt

echo "updating covid19-Data ..."
cd "../../../dailyRe-Data"
git add .
git commit -m "update data"
git push

# reactivate crontab
if  [ ! -z "$cr" ]; then
  echo "restoring crontab from backup"
  crontab crontabBackup.txt
fi
