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
import os
import webbrowser

nltk.download('stopwords')
nltk.download('punkt')
custom_stopwords = stopwords.words('german') + ['bzw', 'sowie']
nlp = spacy.load('de_core_news_md', disable=['parser', 'ner'])
tagger = ht.HanoverTagger('morphmodel_ger.pgz')

REGEX_TO_FILTER = [
    r"\/$",
    r"\{[^\}]+\}",
    r"\n",
    r"\.$",
    r"\:",
    r"\;",
    r"\,",
    r"\!$",
    r"\?$",
    r"\-$",
    r"^\-",
    r"\*innen$",
    r"\*in$",
    r"\*n$",
    r"\*m$",
    r"\*r$",
    r"\:innen$",
    r"\:in$",
    r"\:n$",
    r"\:m$",
    r"\:r$",
    r"\_innen$",
    r"\_in$",
    r"\_n$",
    r"\_m$",
    r"\_r$",
    r"\"",
    r"\_",
    r"^\„",
    r"\“$",
    r"§",
    r"^\{",
    r"\}$",
    r"&",
    r"€",
    r"^\(",
    r"\)$",
    r"\/n$",
    r"\/in$"
]

REGEX_WEBSITES = [  # Since we split the words first, we only need to identify parts of the URL
    r"^www",
    r".de$",
    r".com$",
    r"^http",
    r"^https",
    r".de/",
    r".com/"
]

# Return a list with (processed_word, tag)


def process_word(lemma, tag):
    # Check if regex to filter --> NA, NOTEXT
    for p in REGEX_TO_FILTER:
        matches = re.findall(p, lemma.lower())
        if len(matches) != 0:
            return (pd.NA, 'NOTEXT')

    # Check if webpage, email or number
    for p in REGEX_WEBSITES:
        matches = re.findall(p, lemma.lower())
        if len(matches) != 0:
            return (pd.NA, 'WEBSITE')

    matches = re.findall(r"@", lemma)
    if len(matches) != 0:
        return (pd.NA, 'EMAIL')

    matches = re.findall(r"[0-9]+", lemma)
    if len(matches) != 0:
        return (pd.NA, 'NUMBER')

    # Check if stopword
    if lemma.lower() in custom_stopwords:
        return (pd.NA, 'STOPWORD')

    # Default
    return (lemma, tag)


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
            if len(matches) == 0:
                continue  # Inner loop
            s = s.replace(matches[0], f" {matches[0]} ")
        new_result = s.split(' ')
        new_result = [item for item in new_result if item != '']
        result.extend(new_result)
    return result


def print_df_as_html(df):
    pd.set_option('display.max_seq_items', None)
    html = df.to_html()
    path = os.path.abspath('temp.html')
    url = 'file://' + path
    with open(path, 'w') as f:
        f.write(html)
    webbrowser.open(url)


def interamt_preprocessor(interamt_col, limit=None):
    list_of_dicts = get_all_collection_docs(interamt_col, limit)
    # Pre-filter if not all columns are needed
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['ID', 'Stellenbeschreibung']} for d in list_of_dicts]
    # Remove column references {}
    list_of_filtered_dicts = [
        {
            'ID': entry['ID'],
            # re.sub(r'\{.*?\}', '', entry['Stellenbeschreibung'])
            'Stellenbeschreibung': entry['Stellenbeschreibung']
        }
        for entry in list_of_filtered_dicts]  # --> Replace

    df = pd.DataFrame(list_of_filtered_dicts)

    # Tokenize sentences
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['Stellenbeschreibung'], 'german'), axis=1)
    df.drop('Stellenbeschreibung', axis=1, inplace=True)
    df = df.explode('sentence', ignore_index=True)
    df = df[df['sentence'] != '\u200b']  # Remove \u200b characters
    df['sentence_index'] = df.groupby('ID').cumcount()  # Enumerate Groups

    # First escape placeholders with whitespaces
    df['sentence'] = df.apply(lambda row:
                              placeholder_no_whitespace(row['sentence']), axis=1)

    # Tokenize words, this deletes nothing (other than nltk tokenizers)
    df['word'] = df.apply(lambda row:
                          row['sentence'].split(" "), axis=1)
    df.drop('sentence', axis=1, inplace=True)

    # Seperate special characters for tagging if needed. We need them in the dataset in order to set the text back together later and since the HannoverTagger can't handle spaces. --> Possible request
    df['word'] = df.apply(lambda row:
                          extract_regex_matches(row['word'], REGEX_TO_FILTER), axis=1)

    # Lemmatize words
    df['word'] = df['word'].apply(tagger.tag_sent)
    df = df.explode('word', ignore_index=True)
    df = pd.concat([df, pd.DataFrame(df['word'].values.tolist())], axis=1)
    df.drop('word', axis=1, inplace=True)
    df.rename({df.columns[2]: 'word', df.columns[3]: 'lemma', df.columns[4]: 'tag'}, axis=1, inplace=True)  # Maybe not the cleanest solution

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(
        row['lemma'], row['tag']), axis=1).to_list()

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

    df = interamt_preprocessor(interamt_col, 30)

    # For analysis only
    df_lemma_nona = df.dropna()
    result = count_unique_values(df_lemma_nona, 'lemma')
    print_df_as_html(result)