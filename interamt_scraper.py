# ---------------------------------------------------------------------------- #
#                        Scraper module for Interamt.de                        #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.service import Service
from datetime import datetime
from subprocess import getoutput
from bs4 import BeautifulSoup
import time
import urllib.parse
import pymongo
import json
import requests
from database_wrapper import *

def dicts_equal(d1, d2):
    ''' return True if all keys and values are the same '''
    return all(k in d2 and d1[k] == d2[k]
               for k in d1) \
        and all(k in d1 and d1[k] == d2[k]
                for k in d2)

def get_new_job_ads(conn):
    # Optimization to run on a headless server-side firefox, adjust if other setups are used.
    # Be aware of incompatibilites between selenium and geckodriver, see requirements.txt
    opts = Options()
    opts.binary_location = getoutput(
       'find /snap/firefox -name firefox').split('\n')[-1]
    opts.add_argument('--headless')
    service = Service(executable_path='/usr/local/bin/geckodriver')
    driver = webdriver.Firefox(options=opts, service=service)

    # For Safari
    # driver = webdriver.Safari()

    url = 'https://interamt.de/koop/app/trefferliste'
    driver.get(url)

    # Click cookie button
    time.sleep(3)
    driver.find_element(
        By.XPATH, "//button[contains(@class, 'ia-e-button') and contains(@class, 'ia-e-button--primary') and contains(@class, 'ia-js-cookie-accept__all')]").click()
    time.sleep(2)

    # print("Cookie button clicked")

    soup = BeautifulSoup(driver.page_source, 'html.parser')
    table = soup.find('table')  # find table
    # get number of current job ads
    job_ads_max = int(table.find('caption').find_all('span')[
                      1].text.replace(' Angebote gefunden', ''))
    # print('Total job ads available: ' + str(job_ads_max))
    ADS_PER_LOOP = 10  # constant determined by website
    # plus one since remainder requires a loop, minus one since 10 ads are initially there
    loops = int(job_ads_max / ADS_PER_LOOP)
    if (loops > 300): loops = 300 # Limiter, max. 3000 on initial run
    last_loop_entries = job_ads_max % ADS_PER_LOOP
    # id can change depending of browser
    button_id = table.find('button').get('id')

    # Init list of dicts
    list_of_dicts = list()

    # Get last entry in db
    last_entry_in_db = list(conn.find({}, {'_id': 0, 'ID': 1, 'Eingestellt': 1}).sort(
        [('Eingestellt', pymongo.DESCENDING), ('ID', pymongo.DESCENDING)]))

    if (len(last_entry_in_db) == 0):
        last_entry_in_db = None
    else:
        last_entry_in_db = dict(last_entry_in_db[0])

    # print("DB Check finished: " + str(last_entry_in_db))

    '''
    Optimized code for the initial run.
    '''
    # Load entire table first
    if last_entry_in_db == None:
        for i in range(loops):
            # print('Loop ' + str(i + 1) + ' of ' + str(loops) + ' done.')
            if i == loops:
                break  # Don't click on last run
            if (i != 1):
                try:
                    # Wait max. 20s before throwing exception (JS animation and loading times)
                    driver.implicitly_wait(20)
                    driver.find_element(By.ID, button_id).click()
                    time.sleep(3)  # Min waiting time for animation
                    # print("Clicking: " + str(i) + " out of " + str(loops) + ".")
                except:
                    # print('Driver reloaded.')
                    driver.implicitly_wait(20)
                    driver.refresh()  # keeps the previous state on interamt.de specifically
                    time.sleep(3)
                    try:
                        button_id = table.find('button').get(
                            'id')  # sometimes the button-id changes
                        driver.find_element(By.ID, button_id).click()
                    except:
                        print('Serious trouble here.')
                        print(BeautifulSoup(driver.page_source, 'html.parser'))
                        driver.quit()
                        exit(1)

        soup = BeautifulSoup(driver.page_source, 'html.parser')
        trs = soup.find('table').find('tbody').find_all('tr')
        for tr in trs:
            list_of_dicts.append(get_tr_as_dict(tr))
        driver.quit()
        return list_of_dicts

    '''
    For update use, depends if entries are in the db.
    Click though all entries, save new entries to df in each loop.
    Default sorting of website by adding date and id
    '''
    for i in range(loops):
        # print('Loop ' + str(i + 1) + ' of ' + str(loops) + '_max done.')
        soup = BeautifulSoup(driver.page_source, 'html.parser')
        table_body = soup.find('table').find('tbody')
        trs_last_tens = None
        if (i + 1) == loops:  # last loop
            trs_last_tens = table_body.find_all('tr')[-last_loop_entries:]
        else:
            trs_last_tens = table_body.find_all(
                'tr')[-10:]  # default if not last entry

        finished = False
        for tr in trs_last_tens:
            tr_as_dict = get_tr_as_dict(tr)
            if (last_entry_in_db is not None):
                unique_identifier = dict((k, tr_as_dict[k]) for k in [
                                         'ID', 'Eingestellt'] if k in tr_as_dict)
                unique_identifier['Eingestellt'] = datetime.strptime(
                    unique_identifier['Eingestellt'], '%d.%m.%Y').date().strftime('%Y-%m-%d')
                check = dicts_equal(unique_identifier, last_entry_in_db)
                if (check):
                    finished = True
                    break
            list_of_dicts.append(tr_as_dict)

        if finished:
            break
        if (i != 1):
            try:
                # Wait max. 20s before throwing exception (JS animation and loading times)
                driver.implicitly_wait(20)
                driver.find_element(By.ID, button_id).click()
                time.sleep(3)  # Min waiting time for animation
            except:
                # print('Driver reloaded.')
                driver.implicitly_wait(20)
                driver.refresh()  # keeps the previous state on interamt.de specifically
                time.sleep(3)
                try:
                    button_id = table.find('button').get(
                        'id')  # sometimes the button-id changes
                    driver.find_element(By.ID, button_id).click()
                except:
                    print('Serious trouble here.')
                    print(BeautifulSoup(driver.page_source, 'html.parser'))
                    driver.quit()
                    exit(1)

    '''
    We kept the df instead of a list for a better future proof. IDs might not be unique.
    '''
    driver.quit()
    return list_of_dicts

def get_tr_as_dict(tr):
    data = {}
    for td in tr.find_all('td'):
        label = td['data-label']
        # Handling sub-elements
        subs = td.find_all('span')
        if subs:
            data[label] = subs[0].text.strip()
            continue
        data[label] = td.text.strip()
    return data

def write_to_json_file(list_of_dicts):
    json_string = json.dumps(list_of_dicts)
    f = open('job_table.json', 'w')
    f.write(json_string)
    f.close()

def get_li_as_list(li):
    contents = li.find_all('span', class_='ia-m-desc-list__item-content')
    if (contents is None or len(contents) < 2):
        return None
    term = contents[0].text.strip().replace('\n', ' ')
    value = contents[1].text.strip().replace('\n', ' ')
    return [term, value]

def mongo_authenticate():
    f_open = open('.secrets/mongodb_user.txt', 'r')
    username = f_open.readlines()[0]
    f_open.close()
    username = urllib.parse.quote_plus(username)

    f_open = open('.secrets/mongodb_pwd.txt', 'r')
    password = f_open.readlines()[0]
    f_open.close()
    password = urllib.parse.quote_plus(password)

    # Execution on remote server expected, therefore using localhost and default port.
    client = pymongo.MongoClient(
        'mongodb://%s:%s@localhost:27017' % (username, password), authSource='admin')
    mydb = client['interamtdb']
    mycol = mydb['jobads']

    return mycol

def remove_inline_elements(html_text):
    if html_text == None:
        return None
    NON_BREAKING_ELEMENTS = ['a', 'abbr', 'acronym', 'audio', 'b', 'bdi', 'bdo', 'big', 'button',
                             'canvas', 'cite', 'code', 'data', 'datalist', 'del', 'dfn', 'em', 'embed', 'i', 'iframe',
                             'img', 'input', 'ins', 'kbd', 'label', 'map', 'mark', 'meter', 'noscript', 'object', 'output',
                             'picture', 'progress', 'q', 'ruby', 's', 'samp', 'script', 'select', 'slot', 'small', 'span',
                             'strong', 'sub', 'sup', 'svg', 'template', 'textarea', 'time', 'u', 'tt', 'var', 'video', 'wbr']

    for tag in NON_BREAKING_ELEMENTS:
        for i in html_text.findAll(tag):
            i.unwrap()

    return html_text

def scrape_job_ad(id):
    url = f'https://www.interamt.de/koop/app/stelle?id={id}'

    MAX_RETRIES = 20
    session = requests.Session()
    adapter = requests.adapters.HTTPAdapter(max_retries=MAX_RETRIES)
    session.mount('https://', adapter)
    session.mount('http://', adapter)

    r = session.get(url)

    soup = BeautifulSoup(r.content, 'html.parser')

    stellenbeschreibung_text = soup.find(
        'div', class_='ia-e-richtext ia-h-space--top-l ia-m-job-offer-display-panel ia-h-border--bottom')
    stellenbeschreibung_text = remove_inline_elements(stellenbeschreibung_text)

    # Add newline to li elements
    if stellenbeschreibung_text != None:
        for i in stellenbeschreibung_text.findAll('li'):
            i.insert_after('\n')

    data = {}
    if stellenbeschreibung_text != None:
        data['Stellenbeschreibung'] = stellenbeschreibung_text.getText().strip()

    sidebar = soup.find('div', class_='ia-sidebar')
    if sidebar != None:
        elements = sidebar.findAll('ul', recursive=False)
        lis = []
        for ul in elements:
            lis.extend(ul.findAll('li'))
        for li in lis:
            li_as_list = get_li_as_list(li)
            if (li_as_list is not None):
                # Check if attribute already exists, this happens e.g. when more than one email is provided.
                if li_as_list[0] in data:
                    li_as_list[0] = '_' + li_as_list[0]
                data[li_as_list[0]] = li_as_list[1]

    return data

def remove_duplicates(job_ad):
    '''
    This function removes duplicates of the Interamt Side and other data cleansing stuff.
    '''
    # Behörde --> Behörde/Abteilung
    job_ad.pop('Behörde/Abteilung', None)
    # ID --> INTERAMT Angebots-ID
    job_ad.pop('INTERAMT Angebots-ID', None)
    # Stellenbezeichnung --> Bezeichnung
    job_ad.pop('Bezeichnung', None)
    # Besoldung / Entgelt (seperately)
    job_ad.pop('Besoldung / Entgelt', None)
    # Einsatzort PLZ / Ort (seperately)
    job_ad.pop('Einsatzort PLZ / Ort', None)
    # Entfernung (not relevant)
    job_ad.pop('Entfernung', None)
    # Bewerbungsfrist --> Frist
    job_ad.pop('Frist', None)

    # TODO: Do other cleansing stuff here.
    job_ad['Eingestellt'] = datetime.strptime(
        job_ad['Eingestellt'], '%d.%m.%Y').date().strftime('%Y-%m-%d')

    # '' --> No field
    job_ad = {k: v for k, v in job_ad.items() if v}
    return job_ad

def ireplace(old, new, text):
    '''
    Credit to https://stackoverflow.com/questions/919056/case-insensitive-replace
    '''
    idx = 0
    while idx < len(text):
        index_l = text.lower().find(old.lower(), idx)
        if index_l == -1:
            return text
        text = text[:index_l] + new + text[index_l + len(old):]
        idx = index_l + len(new)
    return text

def replace_with_keys(job_ad_dict):
    '''
    This function replaces value in job description with keys.
    '''
    if 'Stellenbeschreibung' not in job_ad_dict:
        return job_ad_dict
    stellenbeschreibung = job_ad_dict['Stellenbeschreibung']
    for key in job_ad_dict:
        if key == 'Stellenbeschreibung':
            continue
        # TODO: Fix later, also include eins, zwei etc.
        if key == 'Anzahl Stellen':
            continue
        value = job_ad_dict[key]
        stellenbeschreibung = ireplace(
            value, '{' + key.replace(' ', '_') + '}', stellenbeschreibung)
    job_ad_dict['Stellenbeschreibung'] = stellenbeschreibung

    return job_ad_dict

def update_column(conn):
    for row in conn.find():
        conn.update_one({'_id': row['_id']}, {'$set': {'Eingestellt': datetime.strptime(
            row['Eingestellt'], '%d.%m.%Y').date().strftime('%Y-%m-%d')}})

if __name__ == '__main__':
    conn = mongo_authenticate()
    list_of_new_job_ads = get_new_job_ads(conn)

    for i in range(len(list_of_new_job_ads)):
       id = list_of_new_job_ads[i]['ID']
       # Overwriting is ok
       extended_job_ad = {**list_of_new_job_ads[i], **scrape_job_ad(id)}
       # Cleansing
       extended_job_ad = remove_duplicates(extended_job_ad)
       extended_job_ad = replace_with_keys(extended_job_ad)
       # Save to file or db
       conn.insert_one(extended_job_ad)
       # print('Job ad ' + str(i + 1) + ' of ' + str(len(list_of_new_job_ads)) + ' scraped.')
       time.sleep(4)

    print(str(len(list_of_new_job_ads)))