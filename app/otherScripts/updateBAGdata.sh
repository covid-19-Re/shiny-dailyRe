echo "updating BAG Data (polybox sync)"
# for development on local macOS machine (on Linux just install the owncloud cli tools)
# symlink your polybox folder to ../data/BAG
# i.e. ln -s 'path/to/polybox/shared/BAG COVID19 Data' 'path/to/app/data/BAG'
owncloudcmd -n -s ~/BAGdata https://polybox.ethz.ch/remote.php/webdav/BAG%20COVID19%20Data
