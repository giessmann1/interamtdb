#!/bin/bash
# Add this to your crontab 0 23 * * * /bin/sh /home/dau/interamtdb/scraper-starter.sh
filename=scraper-log.txt
if [ ! -f $filename ]
then
    touch $filename
fi

source venv/bin/activate
python3 interamt-scraper.py
