from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.service import Service
from subprocess import getoutput
from bs4 import BeautifulSoup
import time
import pandas as pd
import urllib.parse
import pymongo
import json
import requests

def get_new_job_ads(conn):
    # Optimization to run on a headless server-side firefox, adjust if other setups are used.
    # Be aware of incompatibilites between selenium and geckodriver, see requirements.txt
    opts = Options()
    opts.binary_location = getoutput("find /snap/firefox -name firefox").split("\n")[-1]
    opts.add_argument("--headless")
    service = Service(executable_path="/usr/local/bin/geckodriver")

    driver = webdriver.Firefox(options=opts, service=service)
    url = "https://interamt.de/koop/app/trefferliste"
    driver.get(url)
    
    # Click cookie button
    time.sleep(3)
    driver.find_element(By.XPATH, "//button[contains(@class, 'ia-e-button') and contains(@class, 'ia-e-button--primary') and contains(@class, 'ia-js-cookie-flyout__cta')]").click()
    time.sleep(2)

    soup = BeautifulSoup(driver.page_source, "html.parser")
    table = soup.find("table") # find table
    # get number of current job ads
    job_ads_max = int(table.find("caption").find_all("span")[1].text.replace(" Angebote gefunden", ""))
    print("Total job ads available: " + str(job_ads_max))
    ADS_PER_LOOP = 10 # constant determined by website
    loops = int(job_ads_max / ADS_PER_LOOP) # plus one since remainder requires a loop, minus one since 10 ads are initially there
    last_loop_entries = job_ads_max % ADS_PER_LOOP
    # id can change depending of browser
    button_id = table.find("button").get("id")

    # Init list of dicts
    list_of_dicts = list()

    # Get last entry in db
    last_entry_in_db = list(conn.find({}, {"_id": 0, "ID": 1, "Eingestellt": 1}).sort([("Eingestellt", pymongo.DESCENDING), ("ID", pymongo.DESCENDING)]))
    if (len(last_entry_in_db) == 0):
        last_entry_in_db = None
    else:
        last_entry_in_db = last_entry_in_db[0]

    '''
    Click though all entries, save new entries to df in each loop.
    This approach is a bit slow on intial run, but more efficient for contious use (breaks when all new ads are collected).
    Default sorting of website by adding date and id
    '''
    for i in range(2): #equals max 20 ads for testing
        soup = BeautifulSoup(driver.page_source, "html.parser")
        table_body = soup.find("table").find("tbody")
        trs_last_tens = None
        if (i + 1) == loops: # last loop
            trs_last_tens = table_body.find_all("tr")[-last_loop_entries:]
        else:
            trs_last_tens = table_body.find_all("tr")[-10:] # default if not last entry

        finished = False
        for tr in trs_last_tens:
            tr_as_dict = get_tr_as_dict(tr)
            if (last_entry_in_db is not None):
                unique_identifier = dict((k, tr_as_dict[k]) for k in ["ID", "Eingestellt"] if k in tr_as_dict)
                if (unique_identifier == last_entry_in_db):
                    finished = True
                    break
            list_of_dicts.append(tr_as_dict)

        print("Loop " + str(i + 1) + " of " + str(loops) + "_max done.")
        if finished: break
        if (i != 1):
            driver.find_element(By.ID, button_id).click()
            time.sleep(1) # custom operatio

    driver.quit()

    '''
    We kept the df instead of a list for a better future proof. IDs might not be unique.
    '''
    return list_of_dicts

def get_tr_as_dict(tr):
    data = {}
    for td in tr.find_all("td"):
        label = td["data-label"]
        # Handling sub-elements
        subs = td.find_all("span")
        if subs:
            data[label] = subs[0].text.strip()
            continue
        data[label] = td.text.strip()
    return data

def write_to_json_file(list_of_dicts):
    json_string = json.dumps(list_of_dicts)
    f = open("job_table.json", "w")
    f.write(json_string)
    f.close()

def get_li_as_list(li):
    term = li.find("span", class_ = "ia-m-desc-list__list-term")
    if (term is None):
        return None
    term = term.text.strip()
    value = li.find("span", class_ = "ia-m-desc-list__list-desc").text.strip()
    return [term, value]

def mongo_authenticate():
    host = "localhost"
    port = 27017

    f_open = open(".secrets/mongodb_user.txt",'r')
    username = f_open.readlines()[0]
    f_open.close()
    username = urllib.parse.quote_plus(username)

    f_open = open(".secrets/mongodb_pwd.txt",'r')
    password = f_open.readlines()[0]
    f_open.close()
    password = urllib.parse.quote_plus(password)

    client = pymongo.MongoClient('mongodb://%s:%s@localhost' % (username, password), authSource="admin")
    mydb = client["interamtdb"]
    mycol = mydb["jobads"]

    return mycol

def scrape_job_ad(id):
    url = f"https://www.interamt.de/koop/app/stelle?id={id}"
    r = requests.get(url)
    soup = BeautifulSoup(r.content, "html.parser")
    
    stellenbeschreibung_text = soup.find('div', class_='ia-e-richtext ia-m-section ia-m-job-offer-display-panel ia-h-border--bottom')
    stellenbeschreibung_text = stellenbeschreibung_text.get_text(strip = True, separator="\n")
    data = {}
    data["Stellenbeschreibung"] = stellenbeschreibung_text

    sidebar = soup.find("div", class_ = "ia-sidebar")
    elements =  sidebar.findAll("ul", recursive=False)
    lis = []
    for ul in elements: lis.extend(ul.findAll("li"))
    for li in lis:
        li_as_list = get_li_as_list(li)
        if (li_as_list is not None):
            # Check if attribute already exists, this happens e.g. when more than one email is provided.
            if li_as_list[0] in data:
                li_as_list[0] = "_" + li_as_list[0]
            data[li_as_list[0]] = li_as_list[1]

    return data

if __name__ == "__main__":
    conn = mongo_authenticate()
    list_of_new_job_ads = get_new_job_ads(conn)
    print(list_of_new_job_ads)
    #print("New job ads: " + str(len(list_of_new_job_ads)))

    #extended_list_of_new_job_ads = list()
    #for i in range(len(list_of_new_job_ads)):
     #   id = list_of_new_job_ads[i]["ID"]
     #   extended_job_ad = {**list_of_new_job_ads[i], **scrape_job_ad(id)} # Overwriting is ok
     #   extended_list_of_new_job_ads.append(extended_job_ad)
     #   print("Job ad " + str(i + 1) + " of " + str(len(list_of_new_job_ads)) + " scraped.")
     #   time.sleep(2)

    # TODO: leaning duplicate attributes
    
    # Save to file or db
    #conn.insert_many(extended_list_of_new_job_ads)