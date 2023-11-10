'''
Importing API module provided bundesAPI from: https://github.com/bundesAPI/jobsuche-api
'''
from httpimport import github_repo
with github_repo('bundesAPI', 'jobsuche-api', ref='master'): import api_example as ba_api

if __name__ == "__main__":
    jwt = ba_api.get_jwt()
    result = ba_api.search(jwt["access_token"], "bahn", "berlin")
    print(result['stellenangebote'][0]["refnr"])
    print(ba_api.job_details(jwt["access_token"], result['stellenangebote'][0]["refnr"]))