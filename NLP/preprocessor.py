import spacy
import re
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import stopwords
import nltk
import pandas as pd
import sys
sys.path.append('..')
from database_wrapper import *
# Run this before: python3 -m spacy download de_core_news_md

nltk.download('stopwords')
nltk.download('punkt')
custom_stopwords = stopwords.words('german') + ['bzw']
nlp = spacy.load('de_core_news_md', disable=['parser', 'ner'])


def lemmatize_word(word):
    return ''.join([token.lemma_ for token in nlp(word)]).lower()


# Return a list with (type, processed_word)
def process_word(word):
    # Replace everything not text and lower case
    processed_word = re.sub(r'[^a-zA-ZäöüßÄÖÜ]', '', word).lower()
    if processed_word == '': return ('punctuation', None) # Return punctuations

    # Stopword removal
    processed_word = '' if processed_word in custom_stopwords else processed_word
    if processed_word == '': return ('stopword', None) # Return stopwords

    # Lemmatization, slower than stemming but more accurate
    processed_word = lemmatize_word(processed_word)
    return ('word', processed_word)


def interamt_preprocessor(interamt_col):
    list_of_dicts = get_all_collection_docs(interamt_col, 1)
    # Pre-filter if not all columns are needed
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['ID', 'Stellenbeschreibung']} for d in list_of_dicts]
    # Remove column references {}
    list_of_filtered_dicts = [{'Stellenbeschreibung': re.sub(
        r'\{.*?\}', '', entry['Stellenbeschreibung'])} for entry in list_of_filtered_dicts]

    df = pd.DataFrame(list_of_filtered_dicts)

    # TODO: Somewhere a extra row is added
    
    # Tokenize sentences, this deletes nothing (maybe newlines?)
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['Stellenbeschreibung'], 'german'), axis=1)
    df = df.explode('sentence')

    # Tokenize words, this deletes nothing
    df['word'] = df.apply(lambda row: word_tokenize(
        row['sentence'], 'german'), axis=1)
    df = df.explode('word')

    # Preprocessing
    df[['type', 'processed_word']] = df['word'].apply(process_word).to_list()

    print(df)


if __name__ == '__main__':
    try:
        db = mongo_authenticate('../')
        cols = db.list_collection_names()
        print('Connection working:', cols)
    except Exception as e:
        print('Connection not working.')
        print(e)
        exit(1)

    interamt_col = db['jobads']

    interamt_preprocessor(interamt_col)
