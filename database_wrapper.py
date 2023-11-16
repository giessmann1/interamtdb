import urllib
import pymongo

# Returns connection object at database level


def mongo_authenticate(path):
    # Authentication from remote client possible, add host.txt file under .secrets/ for server IP.
    f_open = open(path+'.secrets/host.txt', 'r')
    host = f_open.readlines()[0]
    f_open.close()
    host = urllib.parse.quote_plus(host)

    port = 27017

    f_open = open(path+'.secrets/mongodb_user.txt', 'r')
    username = f_open.readlines()[0]
    f_open.close()
    username = urllib.parse.quote_plus(username)

    f_open = open(path+'.secrets/mongodb_pwd.txt', 'r')
    password = f_open.readlines()[0]
    f_open.close()
    password = urllib.parse.quote_plus(password)

    client = pymongo.MongoClient(
        'mongodb://%s:%s@%s:%s' % (username, password, host, port), authSource='admin')
    mydb = client['interamtdb']

    return mydb

# Returns all job ads, requires connection object on collection level


def get_all_collection_docs(col, limit=None):
    if limit is not None:
        return list(col.find({}, limit=limit))
    return list(col.find({}))

# Returns number of job ads in collection, requires connection object on collection level


def get_number_of_docs_in_collection(col):
    return col.count_documents(filter={})

# Inserts one document in collection, requires connection object on collection level and dictionary


def insert_one_in_collection(col, doc):
    col.insert_one(doc)

# Inserts multiple document in collection, requires connection object on collection level and list of dictionaries


def insert_many_in_collection(col, list_of_docs):
    col.insert_many(list_of_docs)


# Run the module directly to check if connection works
if __name__ == '__main__':
    try:
        db = mongo_authenticate()
        cols = db.list_collection_names()
        print('Connection working:', cols)
    except Exception as e:
        print('Connection not working.')
        print(e)
        exit(1)
