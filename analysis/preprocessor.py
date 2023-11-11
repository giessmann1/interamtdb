import csv
import re

if __name__ == "__main__":
    with open("stock_index.csv", newline="") as f:
        reader = csv.reader(f)
        companies = list(reader)

    LIST_OF_REMOVAL_WORDS = [
        "a",
        "b",
        "corp",
        "inc",
        "se",
        "class a",
        "class b",
        "vz",
        "ag",
        "asa",
        "oyj",
        "plc",
        "sa",
        "class",
        "n",
        "nv",
        "b",
        "the",
        "farm",
        "d",
        "e",
        "as",
        "cl",
        "kgaa",
        "pref",
        "par",
        "ltd",
        "prf",
        "clas",
        "shs",
        "cor",
        "gruppe",
        "group",
        "dr ing hc f",
        "co",
        "c",
        "holding",
        "holdings",
        "reit",
        "st"
    ]

    companies_cleansed = list()

    for c in companies:
        c = "".join(c)
        c = c.lower()
        c = re.sub("[^0-9a-zA-ZäöüßÄÖÜ- ]", "", c)
        for w in LIST_OF_REMOVAL_WORDS:
            c = re.sub(r"\b%s\b" % w, "", c)
        c = c.replace("  ", " ")
        c = c.replace("dt", "deutsche")
        c = c.strip().title()
        companies_cleansed.append(c)

    companies_cleansed = list(set(companies_cleansed))
    
    with open('company_list_cleansed.csv', mode='wt', encoding='utf-8') as myfile:
        myfile.write('\n'.join(companies_cleansed))
