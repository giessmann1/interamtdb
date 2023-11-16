'''
Importing API module provided bundesAPI from: https://github.com/bundesAPI/jobsuche-api
'''
import time
from langdetect import detect
import sys
sys.path.append('..')
from database_wrapper import *
import csv
import requests
from httpimport import github_repo
with github_repo('bundesAPI', 'jobsuche-api', ref='master'):
    import api_example as ba_api
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

# Overwriting method, as we search by companies not location


def search_by_company(jwt, what):
    '''search for jobs. params can be found here: https://jobsuche.api.bund.dev/'''
    params = (
        ('was', what),  # Title search performs better than company search
        ('size', 100)  # limited to 100, default 25
    )

    headers = {
        'User-Agent': 'Jobsuche/2.9.2 (de.arbeitsagentur.jobboerse; build:1077; iOS 15.1.0) Alamofire/5.4.4',
        'Host': 'rest.arbeitsagentur.de',
        'OAuthAccessToken': jwt,
        'Connection': 'keep-alive',
    }

    response = requests.get('https://rest.arbeitsagentur.de/jobboerse/jobsuche-service/pc/v4/app/jobs',
                            headers=headers, params=params, verify=False)
    return response.json()


if __name__ == '__main__':
    try:
        db = mongo_authenticate('../')
        cols = db.list_collection_names()
        print('Connection working:', cols)
    except Exception as e:
        print('Connection not working.')
        print(e)
        exit(1)

    col_name = 'privateads'
    if col_name not in cols:
        db.create_collection(name=col_name)

    col = db[col_name]

    # Load list of private companies
    with open('company_list_cleansed.csv', newline='') as f:
        reader = csv.reader(f)
        companies = list(reader)

    jwt = ba_api.get_jwt()

    for company in companies:
        # Search by company
        results = search_by_company(jwt['access_token'], *company)
        no_results_check = results.get('maxErgebnisse')

        if no_results_check != 0:
            job_ads = results['stellenangebote']
            print(*company, 'found', len(job_ads),
                  'job ads. Some might not be in German.')

            for ref in job_ads:
                # Filter ads not in German.
                try:
                    # TODO: This can cause exceptions
                    details = ba_api.job_details(
                        jwt['access_token'], ref['refnr'])
                    lang_detect = detect(details['stellenbeschreibung'])
                    if lang_detect == 'de':
                        insert_one_in_collection(col, details)
                except:
                    print('Stellenbeschreibung konnte nicht geladen werden.')
                time.sleep(1)
        else:
            print(*company, 'found 0 job ads.')
        time.sleep(1)

    print('Inserted job ads:', get_number_of_docs_in_collection(col))
