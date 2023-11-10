import urllib
import pymongo

# Returns connection object at collection level
def mongo_authenticate():
    # Authentication from remote client possible, add host.txt file under .secrets/ for server IP.
    f_open = open("../.secrets/host.txt",'r')
    host = f_open.readlines()[0]
    f_open.close()
    host = urllib.parse.quote_plus(host)

    port = 27017

    f_open = open("../.secrets/mongodb_user.txt",'r')
    username = f_open.readlines()[0]
    f_open.close()
    username = urllib.parse.quote_plus(username)

    f_open = open("../.secrets/mongodb_pwd.txt",'r')
    password = f_open.readlines()[0]
    f_open.close()
    password = urllib.parse.quote_plus(password)

    client = pymongo.MongoClient('mongodb://%s:%s@%s:%s' % (username, password, host, port), authSource="admin")
    mydb = client["interamtdb"]
    mycol = mydb["jobads"]

    return mycol

# Returns all job ads, requires connection object
def get_all_job_ads(conn):
    return list(conn.find({}))

# Run the module directly to check if connection works
if __name__ == "__main__":
    try:
        conn = mongo_authenticate()
        cursor = conn.find({})
        print("Connection working:", cursor)
    except Exception as e:
        print("Connection not working.")
        print(e)
    
    print(get_all_job_ads(conn))
