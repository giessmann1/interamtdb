from selenium import webdriver
from selenium.webdriver.common.by import By
from bs4 import BeautifulSoup
import time
import pandas as pd

def get_current_job_ads():
    driver = webdriver.Safari()
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

    # Get column names
    table_body = soup.find("table").find("tbody")
    first_tr = table_body.find_all("tr")[0]
    col_names = list()
    for td in first_tr.find_all("td"):
        col_names.append(td["data-label"])

    # Init df
    df = pd.DataFrame(columns = col_names)

    '''
    Click though all entries, save new entries to df in each loop.
    This approach is a bit slow on intial run, but more efficient for contious use (breaks when all new ads are collected).
    Default sorting of website by adding date
    '''
    for i in range(10):
        soup = BeautifulSoup(driver.page_source, "html.parser")
        table_body = soup.find("table").find("tbody")
        trs_last_tens = None
        if (i + 1) == loops: # last loop
            trs_last_tens = table_body.find_all("tr")[-last_loop_entries:]
        else:
            trs_last_tens = table_body.find_all("tr")[-10:] # default if not last entry

        for tr in trs_last_tens:
            tr_as_list = get_tr_as_list(tr)
            # check if tr id is already in db, then exit booth loops with a print sentence
            df.loc[len(df.index)] =  tr_as_list

        driver.find_element(By.ID, button_id).click()
        time.sleep(1) # custom operation
        print("Loop " + str(i + 1) + " of " + str(loops) + " (max) done.")

    driver.quit()

    # Save to file or db
    write_to_json_file(df)

    '''
    We kept the df instead of a list for a better future proof.
    Maybe the ID is not as unique as we hope for, the more tds will be needed for identification.
    '''
    return df

def get_tr_as_list(tr):
    data = list()
    for td in tr.find_all("td"):
        # Handling sub-elements
        subs = td.find_all("span")
        if subs:
            data.append(subs[0].text.strip())
            continue
        data.append(td.text.strip())
    return data

def write_to_json_file(df):
    json_string = df.to_json(orient="records")
    f = open("job_table.json", "w")
    f.write(json_string)
    f.close()

if __name__ == "__main__":
    get_current_job_ads() # return df of ids
    # scraping new ones, append to db