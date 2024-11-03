#!/bin/bash

# Add this to your crontab 0 23 * * * /bin/bash $HOME/interamtdb/scraper-starter.sh

cd $HOME/interamtdb
filename=scraper-log.txt
if [ ! -f $filename ]
then
    touch $filename
fi

# If you use Python Virtual Environment
source .env/bin/activate
echo $(date +'%Y-%m-%d') | tr "\n" " " >> $filename
python3 interamt_scraper.py >> $filename
deactivate