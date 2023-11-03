#!/bin/bash
# Add this to your crontab 0 23 * * * /bin/sh $HOME/interamtdb/scraper-starter.sh
filename=$HOME/interamtdb/scraper-log.txt
if [ ! -f $filename ]
then
    touch $filename
fi

# If you use Python Virtual Environment
source $HOME/interamtdb/venv/bin/activate
python3 $HOME/interamtdb/interamt-scraper.py