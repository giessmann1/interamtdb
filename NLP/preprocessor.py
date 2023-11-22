import spacy
import re
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import stopwords
import nltk
from HanTa import HanoverTagger as ht
import pandas as pd
import sys
sys.path.append('..')
from database_wrapper import *
# Run this before: python3 -m spacy download de_core_news_md

nltk.download('stopwords')
nltk.download('punkt')
custom_stopwords = stopwords.words('german') + ['bzw']
nlp = spacy.load('de_core_news_md', disable=['parser', 'ner'])
tagger = ht.HanoverTagger('morphmodel_ger.pgz')


# Return a list with (processed_word, tag)
def process_word(lemma, tag):
    # Replace everything not text
    processed_word = re.sub(r'[^a-zA-ZäöüßÄÖÜ]', '', lemma).lower()
    if processed_word == '': return (pd.NA, 'NOTEXT')
    # Stopword removal
    processed_word = '' if processed_word in custom_stopwords else processed_word
    if processed_word == '': return (pd.NA, 'STOPWORD') # Return stopwords

    # Put additional preprocessors here...

    return (processed_word, tag)


def count_unique_values(df, column_name):
    unique_values = df[column_name].value_counts().reset_index()
    unique_values.columns = ['Unique Values', 'Frequency']
    unique_values = unique_values.sort_values(by='Frequency', ascending=False)
    return unique_values


def interamt_preprocessor(interamt_col, limit=None):
    list_of_dicts = get_all_collection_docs(interamt_col, limit)
    # Pre-filter if not all columns are needed
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['ID', 'Stellenbeschreibung']} for d in list_of_dicts]
    # Remove column references {}
    list_of_filtered_dicts = [
    {
        'ID': entry['ID'],
        'Stellenbeschreibung': re.sub(r'\{.*?\}', '', entry['Stellenbeschreibung'])
    }
    for entry in list_of_filtered_dicts]

    df = pd.DataFrame(list_of_filtered_dicts)
    
    # Tokenize sentences, this deletes nothing (maybe newlines?)
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['Stellenbeschreibung'], 'german'), axis=1)
    df.drop('Stellenbeschreibung', axis=1, inplace=True)
    df = df.explode('sentence', ignore_index=True)
    df = df[df['sentence'] != '\u200b'] # Remove \u200b characters
    df['sentence_index'] = df.groupby('ID').cumcount()  # Enumerate Groups

    # Tokenize words, this deletes nothing
    df['word'] = df.apply(lambda row: word_tokenize(
        row['sentence'], 'german'), axis=1)
    df.drop('sentence', axis=1, inplace=True)
    
    # Lemmatize words
    df['word'] = df['word'].apply(tagger.tag_sent)
    df = df.explode('word', ignore_index=True)
    df = pd.concat([df, pd.DataFrame(df['word'].values.tolist())], axis=1)
    df.drop('word', axis=1, inplace=True)
    df.rename({df.columns[2]: 'word', df.columns[3]: 'lemma', df.columns[4]: 'tag'}, axis=1, inplace=True) # Maybe not the cleanest solution

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(row['lemma'], row['tag']), axis=1).to_list()

    return df


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

    df = interamt_preprocessor(interamt_col, 2)

    # For analysis only
    df_lemma_nona = df.dropna()
    result = count_unique_values(df_lemma_nona, 'lemma')
    print(result)