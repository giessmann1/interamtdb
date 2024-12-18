# Interamtdb - Database for Interamt job advertisements
Project to build a research database based on job advertisements from <interamt.de>

❗ This is an actively maintained repository. If you have suggestions for further improvement or find bugs: [Email me](mailto:nico.giessmann@uni-luebeck.de)

## Setup instructions

Create Python virtual environment:
```bash
python3 -m venv .env
source .env/bin/activate
pip3 install -r requirements.txt
```

Create essential files:
```bash
# Create .secrets/ directory
mkdir .secrets/

# Create mongodb_user.txt and mongodb_pwd.txt and set your own username and password (no update in Python scripts necessary). Be aware of newlines, which need to be removed!

echo -n "admin" > .secrets/mongodb_user.txt
echo -n "password" > .secrets/mongodb_pwd.txt
echo -n "localhost" > .secrets/host.txt
```

Docker installation needed, see: https://docs.docker.com/engine/install/.

Starting the db:
```bash
sh startdb.sh
```

For the webdriver, check that automation is enabled/allowed.

## Start scraping

Manual (one-time) start:
```bash
# Make sure venv is activated and db is up running
python3 interamt_scraper.py
```

For automated start with crontab, see scraper-starter.sh
