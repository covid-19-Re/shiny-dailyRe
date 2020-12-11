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
  Rscript --verbose $1 $2 >>messagesRest.Rout 2>>errorsRest.Rout
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
rm *Rest.Rout

echo "running R script to extract linelist data ..."
runRScript format_linelist_data.R

for i in "AFG" "ALB" "ATA" "DZA" "ASM" "AND" "AGO" "ATG" "AZE" "ARG" \
         "AUS" "AUT" "BHS" "BHR" "BGD" "ARM" "ARM" "BRB" "BEL" "BMU" \
         "BTN" "BOL" "BIH" "BWA" "BVT" "BRA" "BLZ" "IOT" "SLB" "VGB" \
         "BRN" "BGR" "MMR" "BDI" "BLR" "KHM" "CMR" "CAN" "CPV" "CYM" \
         "CAF" "LKA" "TCD" "CHL" "CHN" "TWN" "CXR" "CCK" "COL" "COM" \
         "MYT" "COG" "COD" "COK" "CRI" "HRV" "CUB" "CYP" "CYP" "CZE" \
         "BEN" "DNK" "DMA" "DOM" "ECU" "SLV" "GNQ" "ETH" "ERI" "EST" \
         "FRO" "FLK" "SGS" "FJI" "FIN" "ALA" "FRA" "GUF" "PYF" "ATF" \
         "DJI" "GAB" "GEO" "GEO" "GMB" "PSE" "DEU" "GHA" "GIB" "KIR" \
         "GRC" "GRL" "GRD" "GLP" "GUM" "GTM" "GIN" "GUY" "HTI" "HMD" \
         "VAT" "HND" "HKG" "HUN" "ISL" "IND" "IDN" "IRN" "IRQ" "IRL" \
         "ISR" "ITA" "CIV" "JAM" "JPN" "KAZ" "KAZ" "JOR" "KEN" "PRK" \
         "KOR" "KWT" "KGZ" "LAO" "LBN" "LSO" "LVA" "LBR" "LBY" "LIE" \
         "LTU" "LUX" "MAC" "MDG" "MWI" "MYS" "MDV" "MLI" "MLT" "MTQ" \
         "MRT" "MUS" "MEX" "MCO" "MNG" "MDA" "MNE" "MSR" "MAR" "MOZ" \
         "OMN" "NAM" "NRU" "NPL" "NLD" "ANT" "CUW" "ABW" "SXM" "BES" \
         "NCL" "VUT" "NZL" "NIC" "NER" "NGA" "NIU" "NFK" "NOR" "MNP" \
         "UMI" "UMI" "FSM" "MHL" "PLW" "PAK" "PAN" "PNG" "PRY" "PER" \
         "PHL" "PCN" "POL" "PRT" "GNB" "TLS" "PRI" "QAT" "REU" "ROU" \
         "RUS" "RUS" "RWA" "BLM" "SHN" "KNA" "AIA" "LCA" "MAF" "SPM" \
         "VCT" "SMR" "STP" "SAU" "SEN" "SRB" "SYC" "SLE" "SGP" "SVK" \
         "VNM" "SVN" "SOM" "ZWE" "ESP" "SSD" "ESH" "SDN" "SUR" "SJM" \
         "SWZ" "SWE" "SYR" "TJK" "THA" "TGO" "TKL" "TON" "TTO" "ARE" \
         "TUN" "TUR" "TUR" "TKM" "TCA" "TUV" "UGA" "UKR" "MKD" "EGY" \
         "GBR" "GGY" "JEY" "IMN" "TZA" "USA" "VIR" "BFA" "URY" "UZB" \
         "VEN" "WLF" "WSM" "YEM" "ZMB" "XKX" "CHI"
do
	runRScript ReCountry.R "$i"
  if [ "$i" = "CHE" ]
  then
    runRScript makeCHPlots.R
  fi
done

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
