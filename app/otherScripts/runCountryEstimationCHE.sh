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

# summarize data in both apps
cd ../../../dailyRe/app/otherScripts/
runRScript sumData.R
# cd ../../../test-dailyRe/app/otherScripts/
# runRScript sumData.R

# reactivate crontab
if  [ ! -z "$cr" ]; then
  cd "$parent_path"
  echo "restoring crontab from backup"
  crontab crontabBackup.txt
fi
