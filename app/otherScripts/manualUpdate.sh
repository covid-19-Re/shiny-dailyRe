# deactivate crontab
crontab -l > crontabBackup.txt
crontab -r

# update data folder
./updateBAGdata.sh

# run estimation
./runCountryEstimationCHE.sh

# reactivate crontab
crontab crontabBackup.txt
