import urllib
import pymongo
import pandas as pd
import csv
import os

class RUN_OFFLINE:
    def __init__(self, file):
        self.file = file

# Returns connection object at database level
def mongo_authenticate(path: str) -> pymongo.MongoClient:
    if os.environ.get('DOCKER_ENV'):
        host = os.environ.get('MONGO_HOST', 'interamtdb')
        
        # In Docker environment, read credentials from Docker secrets
        try:
            with open('/run/secrets/mongodb-user', 'r') as f_open:
                username = f_open.read().strip()
        except FileNotFoundError:
            raise ValueError("Docker secret 'mongodb-user' not found. Ensure the container has access to the mongodb-user secret.")
        
        try:
            with open('/run/secrets/mongodb-pass', 'r') as f_open:
                password = f_open.read().strip()
        except FileNotFoundError:
            raise ValueError("Docker secret 'mongodb-pass' not found. Ensure the container has access to the mongodb-pass secret.")
        
        try:
            with open('/run/secrets/authSource', 'r') as f_open:
                auth_source = f_open.read().strip()
        except FileNotFoundError:
            auth_source = 'admin'  # Default fallback
    else:
        try:
            with open(f'{path}/.secrets/host.txt', 'r') as f_open:
                host = f_open.readlines()[0].strip()
        except FileNotFoundError:
            host = 'localhost'
        
        try:
            with open(f'{path}/.secrets/mongodb_user.txt', 'r') as f_open:
                username = f_open.readlines()[0].strip()
        except FileNotFoundError:
            raise ValueError("mongodb_user.txt not found in .secrets directory")
        
        try:
            with open(f'{path}/.secrets/mongodb_pwd.txt', 'r') as f_open:
                password = f_open.readlines()[0].strip()
        except FileNotFoundError:
            raise ValueError("mongodb_pwd.txt not found in .secrets directory")
        
        try:
            with open(f'{path}/.secrets/authSource.txt', 'r') as f_open:
                auth_source = f_open.readlines()[0].strip()
        except FileNotFoundError:
            auth_source = 'admin'  # Default fallback

    host = urllib.parse.quote_plus(host)
    username = urllib.parse.quote_plus(username)
    password = urllib.parse.quote_plus(password)
    port = 27017

    client = pymongo.MongoClient(
        f'mongodb://{username}:{password}@{host}:{port}', authSource=auth_source
    )
    return client

def read_csv(file_path, limit=None):
    with open(file_path, 'r', newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        return [row for i, row in enumerate(reader) if limit is None or i < limit]

# Returns all job ads, requires connection object on collection level
def get_all_collection_docs(col, limit=None):
    if type(col) is RUN_OFFLINE:
        return read_csv(col.file, limit)
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

# Returns one column with a filter
def get_one_column_filter(col, column_get, column_filter, filter, limit=None):
    query = {column_filter: filter}
    projection = {column_get: 1, "_id": 0} 
    return list(col.find(query, projection)) if limit is None else list(col.find(query, projection, limit=limit))

# Run the module directly to check if connection works
if __name__ == '__main__':
    try:
        client = mongo_authenticate('./')
        db = client["interamtdb"]
        cols = db.list_collection_names()
        print('Connection working:', cols)
    except Exception as e:
        print('Connection not working.')
        print(e)
        exit(1)