import spacy
import re
from nltk.tokenize import sent_tokenize
from nltk.corpus import stopwords
import nltk
from HanTa import HanoverTagger as ht
import pandas as pd
import sys
sys.path.append('..')
from database_wrapper import *
# Run this before: python3 -m spacy download de_core_news_md
'''
For testing only
'''
from xlwings import view
from pandas.core.base import PandasObject
PandasObject.view = view

nltk.download('stopwords')
nltk.download('punkt')
custom_stopwords = stopwords.words('german') + ['bzw']
nlp = spacy.load('de_core_news_md', disable=['parser', 'ner'])
tagger = ht.HanoverTagger('morphmodel_ger.pgz')

# Return a list with (processed_word, tag)
def process_word(lemma, tag):
    # Check if placeholder
    
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

def placeholder_no_whitespace(input_string):
    pattern = r'\{[^\}]*\s[^\}]*\}'
    matches = re.findall(pattern, input_string)
    for match in matches:
        updated_match = match.replace(' ', '_')
        input_string = input_string.replace(match, updated_match)
    return input_string

def extract_regex_matches(strings, regex_patterns):
    result = []
    for s in strings:
        for pattern in regex_patterns:
            matches = re.findall(pattern, s)
            if len(matches) == 0: continue
            s = s.replace(matches[0], f" {matches[0]} ")
        result.extend(s.split())

    return result

def interamt_preprocessor(interamt_col, limit=None):
    list_of_dicts = get_all_collection_docs(interamt_col, limit)
    # Pre-filter if not all columns are needed
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['ID', 'Stellenbeschreibung']} for d in list_of_dicts]
    # Remove column references {}
    list_of_filtered_dicts = [
    {
        'ID': entry['ID'],
        'Stellenbeschreibung': entry['Stellenbeschreibung'] # re.sub(r'\{.*?\}', '', entry['Stellenbeschreibung'])
    }
    for entry in list_of_filtered_dicts] # --> Replace

    df = pd.DataFrame(list_of_filtered_dicts)

    # Tokenize sentences
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['Stellenbeschreibung'], 'german'), axis=1)
    df.drop('Stellenbeschreibung', axis=1, inplace=True)
    df = df.explode('sentence', ignore_index=True)
    df = df[df['sentence'] != '\u200b'] # Remove \u200b characters
    df['sentence_index'] = df.groupby('ID').cumcount()  # Enumerate Groups

    # First escape placeholders with whitespaces
    df['sentence'] = df.apply(lambda row: 
        placeholder_no_whitespace(row['sentence']), axis=1)
    
    # Tokenize words, this deletes nothing (other than nltk tokenizers)
    df['word'] = df.apply(lambda row: 
        row['sentence'].split(' '), axis=1)
    df.drop('sentence', axis=1, inplace=True)

    # Special characters extraction. We keep those in the dataset, because we later merge the text together.
    regex = [r".*@.*.de", r"www.*.de", r"\{[^\}]+\}", r"\.", r"\,", r"\\n", r"\!", r"\?"] # TODO: Links and E-Mails don't replace correctly
    df['word'] = df.apply(lambda row: 
        extract_regex_matches(row['word'], regex), axis=1)
    
    # Lemmatize words
    df['word'] = df['word'].apply(tagger.tag_sent)
    df = df.explode('word', ignore_index=True)
    df = pd.concat([df, pd.DataFrame(df['word'].values.tolist())], axis=1)
    df.drop('word', axis=1, inplace=True)
    df.rename({df.columns[2]: 'word', df.columns[3]: 'lemma', df.columns[4]: 'tag'}, axis=1, inplace=True) # Maybe not the cleanest solution

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(row['lemma'], row['tag']), axis=1).to_list() # TODO: Correct labeling of placeholders, e-mails, links etc. Remove gendern in lemma

    df.view()

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
    #df_lemma_nona = df.dropna()
    #result = count_unique_values(df_lemma_nona, 'lemma')
    #result.view()