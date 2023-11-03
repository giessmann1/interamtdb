# Interamtdb - Database for Interamt job advertisements
Project to build a research database based on job advertisements from <interamt.de>

## Setup instructions

Create Python virtual environment:
```bash
python3 -m venv venv
source venv/bin/activate
pip3 install -r requirements.txt
```

Create essential files:
```bash
# Create .secrets/ directory
mkdir .secrets/

# Create mongodb_user.txt and mongodb_pwd.txt and set your own username and password (no update in Python scripts necessary). Be aware of newlines, which need to be removed!

echo "username" > .secrets/mongodb_user.txt
echo "password" > .secrets/mongodb_pwd.txt
```

Starting the db:
```bash
sh startdb.sh
```

## Start scraping

Manual (one-time) start:
```bash
# Make sure venv is activated and db is up running
python3 interamt-scraper.py
```

For automated start with crontab, see scraper-starter.sh
