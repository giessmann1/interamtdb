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

nltk.download('stopwords', quiet=True)
nltk.download('punkt', quiet=True)
custom_stopwords = stopwords.words('german') + ['bzw', 'sowie', 'ca', 'ggf', 'ab', 'incl', 'ggfs', 'z.b', 'je', 'inkl', 'u.a', 'o.g', 'zt', 'z.zt', 'usw', 'etwa', 'd.h']
nlp = spacy.load('de_core_news_md', disable=['parser', 'ner'])
tagger = ht.HanoverTagger('morphmodel_ger.pgz')
pd.set_option('display.max_seq_items', None)

REGEX_TO_FILTER = [
    r"\t",
    r"\{[^\}]+\}",
    r"\s+[a-zA-Z0-9äÄöÖüÜß]+\/",
    r"\-[^a-zA-Z0-9äÄöÖüÜß]",
    r"^[a-zA-Z0-9äÄöÖüÜß]+[.-]+[a-zA-Z0-9äÄöÖüÜß]+\/",  # This may cause trouble
    r"^[a-zA-Z0-9äÄöÖüÜß]+\)",
    r"\.\/",
    r"\/\s+",
    r"\/$",
    r"^\/",
    r"\n",
    r"^\.{3}",
    r"\.+\s+",
    r"\:\s+",
    r"\:$",
    r"\;",
    r"\,\s+",
    r"\s+\,",
    r"\!$",
    r"\?$",
    r"\-$",
    r"\-\s+",
    r"^\-",
    r"\s+\-",
    r"\-\s*$",

    # Gender suffixes
    r"\*innen",
    r"\*innen",
    r"[\*:\_](in|innen|n|m|r)\s*",
    r"\/(-r|-mann|r)\s*",

    r"\*zze",
    r"\*nja",
    r"\*nja",

    r"\"",
    r"^\„",
    r"\“$",
    r"^\“",
    r"\”$",
    r"§",
    r"&",
    r"€",
    r"\/n$",
    r"\/in$",
    r"\s+\(",
    r"^\(",
    r"\)\s+",
    r"\)$",
    r"\($",
    r"\,$",
    r"\.+$",
    r"\_{2,}",
    r"\’$",
    r"\’\s+",
    r"\.\‘$",
    r"^\‚",
    r"\‘$",
    # Needs to be at the end
    r"^[a-zA-Z0-9äÄöÖüÜß]+\/",
    r"^[a-zA-Z0-9äÄöÖüÜß]+\*",
]

REGEX_WEBSITES = [  # Since we split the words first, we only need to identify parts of the URL
    r"^www",
    r"\.(de|com)(\/|$)",
    r"^https?",
]

REGEX_EMAIL = [
    r"@",
    r"\(at\)",
]

REGEX_ROMAN = r"^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$"

def process_word(lemma, tag):
    # Return a list with (processed_word, tag)

    # Check if regex to filter --> NA, NOTEXT
    for p in REGEX_TO_FILTER:
        matches = re.findall(p, lemma.lower())
        if len(matches) != 0:
            return (pd.NA, 'NOTEXT')

    # Check if only one character
    if len(lemma) <= 1:
        return (pd.NA, 'SINGLECHAR')

    # Check if webpage, email or number
    for p in REGEX_WEBSITES:
        matches = re.findall(p, lemma.lower())
        if len(matches) != 0:
            return (pd.NA, 'WEBSITE')

    for p in REGEX_EMAIL:
        matches = re.findall(p, lemma)
        if len(matches) != 0:
            return (pd.NA, 'EMAIL')

    matches = re.findall(r"[0-9]+", lemma)
    if len(matches) != 0:
        return (pd.NA, 'NUMBER')

    matches = re.findall(REGEX_ROMAN, lemma.upper())
    if len(matches) != 0:
        return (pd.NA, 'ROMAN')

    # Check if stopword
    if lemma.lower() in custom_stopwords:
        return (pd.NA, 'STOPWORD')

    # Default
    lemma = lemma.lower()
    # lemma = re.sub(r"[^a-zA-ZäÄöÖüÜß]", '', lemma)

    return (lemma, tag)


def frequency_by_columns(df, column1, column2):
    result = df.groupby([column1, column2]).size().reset_index(name='Frequency')
    result = result.sort_values(by='Frequency', ascending=False)
    return result


def frequency_by_one_column(dataframe, column_name):
    counts = dataframe[column_name].value_counts()
    return pd.DataFrame(
        {column_name: counts.index, 'Frequency': counts.values}
    )


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
        s = re.sub('\xa0', ' ', s)  # Replace '\xa0' characters
        s = re.sub('…', '...', s)  # Replace U+2026 characters
        s = re.sub('–', '-', s)  # Replace english version
        for pattern in regex_patterns:
            s = re.sub(pattern, r' \g<0> ', s)
        new_result = s.strip().split(' ')
        new_result = [item for item in new_result if item != '']
        result.extend(new_result)
    return result


def view_df_as_html(df):
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

    df = pd.DataFrame(list_of_filtered_dicts)
    print("Dataframe loaded.")

    # Tokenize sentences
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['Stellenbeschreibung'], 'german'), axis=1)
    df.drop('Stellenbeschreibung', axis=1, inplace=True)
    df = df.explode('sentence', ignore_index=True)
    df = df[df['sentence'] != '\u200b']  # Remove solo \u200b characters
    df['sentence_index'] = df.groupby('ID').cumcount()  # Enumerate Groups
    print("Sentences tokenized.")

    # First escape placeholders with whitespaces
    df['sentence'] = df.apply(lambda row: placeholder_no_whitespace(row['sentence']), axis=1)
    print("Placeholder spaces replaced.")

    # Tokenize words, this deletes nothing (other than nltk tokenizers)
    df['word'] = df.apply(lambda row: str(row['sentence']).split(" "), axis=1)
    df.drop('sentence', axis=1, inplace=True)
    print("Words tokenized.")

    # Seperate special characters for tagging if needed. We need them in the dataset in order to set the text back together later and since the HannoverTagger can't handle spaces. --> Possible request
    df['word'] = df.apply(lambda row: extract_regex_matches(row['word'], REGEX_TO_FILTER), axis=1)
    print("Special character seperated.")

    # Lemmatize words
    df['word'] = df['word'].apply(tagger.tag_sent)
    df = df.explode('word', ignore_index=True)
    df = pd.concat([df, pd.DataFrame(df['word'].values.tolist())], axis=1)
    df.drop('word', axis=1, inplace=True)
    df.rename({df.columns[2]: 'word', df.columns[3]: 'lemma', df.columns[4]: 'tag'}, axis=1, inplace=True)  # Maybe not the cleanest solution
    print("Lemmatization done.")

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(
        row['lemma'], row['tag']), axis=1).to_list()
    print("Preprocessing done.")

    return df


def ba_preprocessor(ba_col, limit=None):
    list_of_dicts = get_all_collection_docs(ba_col, limit)
    print("Dataframe loaded.")
    return pd.DataFrame(list_of_dicts)


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
    ba_col = db['privateads']

    limit = None if len(sys.argv) == 1 else int(sys.argv[1])
    '''
    # Interamt
    df = interamt_preprocessor(interamt_col, limit)
    df['word'] = df['word'].apply(lambda x: x.replace('\n', '\\n'))
    df.to_csv('interamt_vocab.csv', quoting=csv.QUOTE_ALL, index=False)
    '''

    # BA
    df = db_preprocessor(ba_col, limit)
    view_df_as_html(df)

    # For testing purposes
    # df_lemma_nona = df.dropna()
    # result = frequency_by_one_column(df_lemma_nona, 'lemma')
    # result = frequency_by_columns(df_lemma_nona, 'lemma', 'tag')