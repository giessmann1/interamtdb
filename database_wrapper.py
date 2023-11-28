import urllib
import pymongo

# Returns connection object at database level
def mongo_authenticate(path):
    with open(f'{path}.secrets/host.txt', 'r') as f_open:
        host = f_open.readlines()[0]
    host = urllib.parse.quote_plus(host)

    port = 27017
    with open(f'{path}.secrets/mongodb_user.txt', 'r') as f_open:
        username = f_open.readlines()[0]
    username = urllib.parse.quote_plus(username)
    with open(f'{path}.secrets/mongodb_pwd.txt', 'r') as f_open:
        password = f_open.readlines()[0]
    password = urllib.parse.quote_plus(password)

    client = pymongo.MongoClient(
        f'mongodb://{username}:{password}@{host}:{port}', authSource='admin'
    )
    return client['interamtdb']

# Returns all job ads, requires connection object on collection level
def get_all_collection_docs(col, limit=None):
    return list(col.find({}, {'_id':0})) if limit is None else list(col.find({}, {'_id':0}, limit=limit))


def get_one_column(col, column, limit=None):
    return list(col.find({}, {column:1, '_id':0})) if limit is None else list(col.find({}, {column:1, '_id':0}, limit=limit))


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
