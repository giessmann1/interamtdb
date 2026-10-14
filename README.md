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
echo -n "admin" > .secrets/authSource.txt
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

## Citation

If you use this project in your research, please cite it:

```bibtex
@article{Giessmann2026,
author = {Gießmann, Nico and Drathschmidt, Nicolas},
title = {Between the Lines: Decoding Signaling Differences in Public and Private Sector Job Advertisements},
journal = {Public Administration Review},
volume = {n/a},
number = {n/a},
pages = {},
keywords = {computational research methods, employer branding, job advertisements, public service motivation, recruitment messaging, signaling theory},
doi = {https://doi.org/10.1111/puar.70223},
url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/puar.70223},
eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/puar.70223},
abstract = {ABSTRACT To compete in the “war for talent,” public sector employers must demonstrate advantages over private employers. While public service motivation (PSM) predicts sector preference, scholars disagree on whether public employers (can) effectively signal PSM in job advertisements. Consequently, this article examines how public and private job advertisements differ in language. We collected 5000 advertisements from both sectors and performed natural language processing (NLP) and qualitative content analysis. Both sectors use specialized language that can narrow the applicant pool. PSM signals appear more often in the public sector, but affective motives are rarely communicated; instead, self-sacrifice is framed as a legal obligation. Public sector advertisements adopt a rational tone instead of explaining purpose and societal value. Legally mandated, saturated signals may be perceived as low-cost and ineffective. The findings explain the mixed results concerning PSM signaling and underscore the gap between theory and practice in PSM-related recruitment efforts.}
}
```
