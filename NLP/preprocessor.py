# ---------------------------------------------------------------------------- #
#         Script for preprocessing job ads from Interamt and BA dataset        #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

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
from sector_tagger import *
from interamt_scraper import ireplace
from helper import view_df_as_html, frequency_by_columns, frequency_by_one_column, bcolors
import csv
# Run this before: python3 -m spacy download de_core_news_md

nltk.download('stopwords', quiet=True)
nltk.download('punkt', quiet=True)
custom_stopwords = stopwords.words('german') + ['bzw', 'sowie', 'ca', 'ggf', 'ab', 'incl', 'ggfs', 'z.b', 'je', 'inkl', 'u.a', 'o.g', 'zt', 'z.zt', 'usw', 'etwa', 'd.h', 'i.v.m', 'ff', 'zzgl', 'zzt', 'zz']
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
    r"\-innen",
    r"[\*:\_](in|innen|n|m|r)\s*",
    r"\/(-r|-mann|r)\s*",

    r"\*zze",
    r"\*nja",
    r"\*nja",
    r"\*"

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

    lemma = lemma.lower()

    # Newlines
    if len(re.findall(r"\n", lemma)) != 0:
        return (pd.NA, 'NEWLINE')

    if len(re.findall(r"\t", lemma)) != 0:
        return (pd.NA, 'TAB')

    # Check if only one character
    if len(lemma) <= 1:
        return (pd.NA, 'SINGLECHAR')

    # Check if hashtag
    if len(re.findall(r"#", lemma)) != 0:
        return (pd.NA, 'HASHTAG')

    for p in REGEX_EMAIL:
        matches = re.findall(p, lemma)
        if len(matches) != 0:
            return (pd.NA, 'EMAIL')

    # Check if webpage, email or number
    for p in REGEX_WEBSITES:
        matches = re.findall(p, lemma)
        if len(matches) != 0:
            return (pd.NA, 'WEBSITE')

    matches = re.findall(r"[0-9]+", lemma)
    if len(matches) != 0:
        return (pd.NA, 'NUMBER')

    matches = re.findall(REGEX_ROMAN, lemma.upper())
    if len(matches) != 0:
        return (pd.NA, 'ROMAN')

    # Check if stopword
    if lemma in custom_stopwords:
        return (pd.NA, 'STOPWORD')

    # Default
    matches = re.findall(r"[^a-zA-ZäÄöÖüÜß]", lemma) # Problem with -innen / -in
    if len(matches) != 0:
        return (pd.NA, 'NOTEXT')

    # Lose letters
    if tag == 'XY':
        return (pd.NA, 'NOTEXT')

    return (lemma, tag)

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
        s = re.sub('\xad', ' ', s)  # Replace '\xad' characters
        s = re.sub('\x09', r'\t', s)
        s = re.sub('…', '...', s)  # Replace U+2026 characters
        s = re.sub('–', '-', s)  # Replace english version
        for pattern in regex_patterns:
            s = re.sub(pattern, r' \g<0> ', s)
        new_result = s.strip().split(' ')
        new_result = [item for item in new_result if item != '']
        result.extend(new_result)
    return result

def interamt_preprocessor(interamt_col, limit=None):
    list_of_dicts = get_all_collection_docs(interamt_col, limit)
    # Pre-filter if not all columns are needed
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['ID', 'Behörde', 'Stellenbeschreibung', 'Eingestellt']} for d in list_of_dicts]
    df = pd.DataFrame(list_of_filtered_dicts)
    df['Stellenbeschreibung'] = df['Stellenbeschreibung'].replace('', pd.NA)
    df = df.dropna() # Removes 'Stellenbeschreibung' when None --> eg. references to websites
    print("Dataframe loaded.")

    # Remove employers not public
    employers_to_include = frequency_by_one_column(df, 'Behörde')
    sum_employers = len(employers_to_include.index)
    employers_to_include['Tags'] = employers_to_include.apply(lambda row: ' '.join(find_tags(str(row['Behörde']), public_tagger)), axis=1)
    employers_to_include = employers_to_include.replace(r'^\s*$', pd.NA, regex=True)
    employers_to_include = employers_to_include.dropna()
    sum_found = sum_employers - len(employers_to_include.index)
    employers_to_include_list = employers_to_include['Behörde'].tolist()
    df = df[df['Behörde'].isin(employers_to_include_list)]
    # df = df.drop('Behörde', axis=1)
    print("Non-public employers excluded.")
    print(f"{sum_found} of {sum_employers} ({round(sum_found / sum_employers * 100,2)}%) are removed.")

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
    df.rename({df.columns[3]: 'word', df.columns[4]: 'lemma', df.columns[5]: 'tag'}, axis=1, inplace=True)  # Maybe not the cleanest solution
    print("Lemmatization done.")

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(
        row['lemma'], row['tag']), axis=1).to_list()
    print("Preprocessing done.")

    return df

# TODO: Harmonize with same method from interamt_scraper.py
def replace_with_keys(job_ad_dict):
    '''
    This function replaces value in job description with keys.
    '''
    if 'stellenbeschreibung' not in job_ad_dict:
        return ''
    stellenbeschreibung = job_ad_dict['stellenbeschreibung']
    for key in job_ad_dict:
        if key == 'stellenbeschreibung':
            continue
        value = job_ad_dict[key]
        if value in (None, ""):
            continue
        stellenbeschreibung = ireplace(
            value, '{' + key.replace(' ', '_') + '}', stellenbeschreibung)

    return stellenbeschreibung

def ba_preprocessor(ba_col, limit=None):
    list_of_dicts = get_all_collection_docs(ba_col, limit)
    list_of_filtered_dicts = [
        {key: d.get(key) for key in ['refnr', 'modifikationsTimestamp', 'arbeitgeber', 'branche', 'eintrittsdatum', 'titel', 'beruf', 'stellenbeschreibung', 'tarifvertrag', 'tarifvertrag', 'externeUrl']} for d in list_of_dicts]
    df = pd.DataFrame(list_of_filtered_dicts)
    df['stellenbeschreibung'] = df['stellenbeschreibung'].replace('', pd.NA)
    df = df.dropna() # Removes 'stellenbeschreibung' when None --> eg. references to websites
    print("Dataframe loaded.")

    # Remove public and non-profit employers
    employers_to_exclude = frequency_by_one_column(df, 'arbeitgeber')
    sum_employers = len(employers_to_exclude.index)
    employers_to_exclude['Tags'] = employers_to_exclude.apply(lambda row: ' '.join(find_tags_two_methods(str(row['arbeitgeber']), public_tagger, nonprofit_tagger)), axis=1)
    employers_to_exclude = employers_to_exclude.replace(r'^\s*$', pd.NA, regex=True)
    employers_to_exclude = employers_to_exclude.dropna()
    sum_found = len(employers_to_exclude.index)
    merged_df = df.merge(employers_to_exclude, on='arbeitgeber', how='left', indicator=True)
    df = merged_df[merged_df['_merge'] == 'left_only'].drop(columns=['_merge'])
    df = df.drop('Frequency', axis=1)
    df = df.drop('Tags', axis=1)
    print("Public and Non-profit employers excluded.")
    print(f"{sum_found} of {sum_employers} ({round(sum_found / sum_employers * 100,2)}%) are removed.")

    # Replace key
    df['stellenbeschreibung'] = df.apply(lambda row: replace_with_keys(dict(row)), axis=1)
    print("Keys replaced.")
    df = df[['refnr', 'arbeitgeber', 'stellenbeschreibung']]
    df.rename({'refnr': 'ID'}, axis=1, inplace=True) # The same as interamt ads

    # Tokenize sentences
    df['sentence'] = df.apply(lambda row: sent_tokenize(
        row['stellenbeschreibung'], 'german'), axis=1)
    df.drop('stellenbeschreibung', axis=1, inplace=True)
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
    df.rename({df.columns[3]: 'word', df.columns[4]: 'lemma', df.columns[5]: 'tag'}, axis=1, inplace=True)  # Maybe not the cleanest solution
    print("Lemmatization done.")

    # Preprocessing
    df[['lemma', 'tag']] = df.apply(lambda row: process_word(
        row['lemma'], row['tag']), axis=1).to_list()
    print("Preprocessing done.")

    return df

if __name__ == '__main__':
    try:
        db = mongo_authenticate('../')
        cols = db.list_collection_names()
        print('Connection working:', cols)
        interamt_col = db['jobads']
        ba_col = db['privateads']
    except Exception as e:
        print('Connection not working.')
        interamt_col = RUN_OFFLINE("../public_ads_5000.csv")
        ba_col = RUN_OFFLINE("../private_ads_5000.csv")
        
    limit = None if len(sys.argv) == 1 else int(sys.argv[1])

    # Interamt
    print(bcolors.OKBLUE + "Interamt preprocessing started..." + bcolors.ENDC)
    df = interamt_preprocessor(interamt_col, limit)

    # view_df_as_html(df)
    df['word'] = df['word'].apply(lambda x: x.replace('\n', '\\n'))
    df['word'] = df['word'].apply(lambda x: x.replace('\t', '\\t'))
    df.to_csv('public_ads.csv', quoting=csv.QUOTE_ALL, index=False)
    
        # BA
    print(bcolors.OKBLUE + "BA preprocessing started..." + bcolors.ENDC)
    df = ba_preprocessor(ba_col, limit)

    # view_df_as_html(df)
    df['word'] = df['word'].apply(lambda x: x.replace('\n', '\\n'))
    df['word'] = df['word'].apply(lambda x: x.replace('\t', '\\t'))
    df.to_csv('private_ads.csv', quoting=csv.QUOTE_ALL, index=False)

    '''
    # For testing purposes
    df_lemma_nona = df.dropna()
    result = frequency_by_one_column(df_lemma_nona, 'lemma')
    result = frequency_by_columns(df_lemma_nona, 'lemma', 'tag')
    '''