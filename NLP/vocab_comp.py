# ---------------------------------------------------------------------------- #
#           Vocabulary comparsion between public and private job ads           #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

# ---------------------------------- Imports --------------------------------- #
import pandas as pdv
from helper import *
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation
from gensim.models import CoherenceModel
import matplotlib.pyplot as plt
import numpy as np
from sklearn.preprocessing import MaxAbsScaler
from gensim.models import CoherenceModel
from gensim.corpora import Dictionary
import warnings

# --------------------------------- Settings --------------------------------- #
# Suppress RuntimeWarning in Gensim
warnings.filterwarnings("ignore", category=RuntimeWarning)

# --------------------------------- Constants -------------------------------- #
PUBLIC_VOCAB_FILE = "public_vocab.csv"
PRIVATE_VOCAB_FILE = "private_vocab.csv"
WORDS_BY_EMPLOYER_MIN = 1
SAMPLE_SIZE = 5000

# ---------------------------------- Methods --------------------------------- #
def sample_first_n_ids(vocab):
    unique_ids = vocab['ID'].unique()
    return unique_ids[:SAMPLE_SIZE]


def clean_vocab(vocab, employer_column):
    # Sampling
    vocab = vocab[vocab['ID'].isin(sample_first_n_ids(vocab))]
    # Vocab cleansing
    vocab = vocab.dropna()
    vocab.loc[:, 'tag'] = vocab['tag'].replace(regex=r'\(.+\)', value='')
    vocab.reset_index(inplace = True, drop = True)
    vocab["index_saved"] = vocab.index

    # Create a copy to avoid SettingWithCopyWarning
    words = vocab.copy()
    # Group by 'lemma' and 'tag', then summarize
    words = words.groupby(['lemma', 'tag']).agg(
        word_by_employers = (employer_column, 'nunique')
    ).reset_index()
    '''
    We filter out employer-specific or low usage terms for better interpretability of the model,
    for practical use those should stay in the vocab since they increase the model accuracy (but also its size).
    '''
    # Filter rows where 'word_by_employers' is greater than 1
    words = words[words['word_by_employers'] > WORDS_BY_EMPLOYER_MIN]
    # Drop the 'word_by_employers' column
    words = words.drop(columns=['word_by_employers'])

    # Only keep relevant words in vocab
    vocab = pd.merge(vocab, words, on=['lemma', 'tag'], how='inner').sort_values(by = "index_saved")
    vocab = vocab.drop(columns=["index_saved"]).reset_index(drop=True)

    return vocab
    
# -------------------------------- Main script ------------------------------- #
if __name__ == '__main__':
    # Public
    public_vocab = pd.read_csv(PUBLIC_VOCAB_FILE)
    public_vocab = clean_vocab(public_vocab, "Behörde")
    public_vocab_reduced = public_vocab[["ID", "lemma"]]

    public_vocab_grouped = public_vocab_reduced.groupby('ID')['lemma'].apply(lambda x: ' '.join(x)).reset_index()

    tfidf_vectorizer = TfidfVectorizer()
    tfidf_matrix = tfidf_vectorizer.fit_transform(public_vocab_grouped['lemma'])
    # Scale the TF-IDF matrix using MaxAbsScaler
    scaler = MaxAbsScaler()
    tfidf_matrix_scaled = scaler.fit_transform(tfidf_matrix)
    
    count_vectorizer = CountVectorizer()
    count_matrix = count_vectorizer.fit_transform(public_vocab_grouped['lemma'])
    count_matrix_scaled = scaler.fit_transform(count_matrix)
    
    # Fit LDA model with the chosen number of topics
    num_topics = 30  # Adjust as needed
    lda_model = LatentDirichletAllocation(n_components = num_topics, random_state=42)
    # Assuming lda_model is your fitted LatentDirichletAllocation model
    lda_topic_matrix = lda_model.fit_transform(tfidf_matrix_scaled)
    lda_model.components_ = lda_model.components_ / lda_model.components_.sum(axis=1)[:, np.newaxis]

    # Get feature names from TF-IDF vectorizer
    feature_names = np.array(tfidf_vectorizer.get_feature_names_out())

    # Display the top terms for each topic in the grid
    fig, axes = plt.subplots(10, 3, figsize=(15, 15), sharex=True)  # Adjust the grid size based on your preference

    for topic_idx, ax in enumerate(axes.flatten()):
        if topic_idx < num_topics:
            top_terms_idx = lda_model.components_[topic_idx].argsort()[::-1][:10]  # Display top 10 terms in descending order
            sorted_indices = np.argsort(lda_model.components_[topic_idx])[::-1]

            top_terms = feature_names[top_terms_idx][::-1]
            
            ax.barh(range(len(top_terms)), lda_model.components_[topic_idx][top_terms_idx][::-1], align='center')
            ax.set_yticks(range(len(top_terms)))
            ax.set_yticklabels(top_terms)
            ax.set_xlabel('Normalized TF-IDF Score')
            ax.set_title(f'Topic #{topic_idx + 1}')

            # Add TF-IDF scores on the x-axis
            for i, val in enumerate(lda_model.components_[topic_idx][top_terms_idx]):
                ax.text(val, i, f'{val:.3f}', ha='left', va='center')

        else:
            ax.axis('off')  # Turn off empty subplots


    plt.tight_layout()
    # Save the output to a PNG file
    plt.savefig('lda_topics_visualization_normalized.png')


