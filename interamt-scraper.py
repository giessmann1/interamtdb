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

    # click though all entries
    for i in range(9):
        driver.find_element(By.ID, button_id).click()
        time.sleep(1) # custom operation
        print("Loop " + str(i + 1) + " of " + str(loops) + " done.")

    soup = BeautifulSoup(driver.page_source, "html.parser")
    table_body = soup.find("table").find("tbody")
    trs = table_body.find_all("tr")

    driver.quit()

    # Save data
    col_names = list()
    for td in trs[0].find_all("td"):
        col_names.append(td["data-label"])

    df = pd.DataFrame(columns = col_names)

    for tr in trs:
        data = list()
        for td in tr.find_all("td"):
            # Handling sub-elements
            subs = td.find_all("span")
            if subs:
                data.append(subs[0].text.strip())
                continue
            data.append(td.text.strip())
        df.loc[len(df.index)] = data
    
    return df

if __name__ == "__main__":
    # job_table = get_current_job_ads()
    job_table = pd.read_csv("job_table.csv")
