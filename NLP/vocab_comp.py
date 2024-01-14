# ---------------------------------------------------------------------------- #
#           Vocabulary comparsion between public and private job ads           #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

# ---------------------------------- Imports --------------------------------- #
import pandas as pdv
from helper import *
from sklearn.feature_extraction.text import TfidfVectorizer
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
SAMPLE_SIZE = 1000

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


def calculate_perplexity_scores(X, min_topics, max_topics):
    perplexity_scores = []
    for num_topics in range(min_topics, max_topics + 1):
        lda_model = LatentDirichletAllocation(n_components=num_topics, random_state=42)
        lda_model.fit(X)
        perplexity_scores.append(lda_model.perplexity(X))
    return perplexity_scores


# Function to convert TF-IDF matrix to Gensim Dictionary
def convert_to_gensim_dict(df):
    gensim_dict = Dictionary([df.columns])
    return gensim_dict


# Function to calculate coherence scores for a range of topics (k)
def calculate_coherence_scores(X, min_topics, max_topics):
    coherence_scores = []
    
    for num_topics in range(min_topics, max_topics + 1):
        lda_model = LatentDirichletAllocation(n_components=num_topics, random_state=42)
        lda_model.fit(X)

        # Convert TF-IDF matrix to Gensim Dictionary
        gensim_dict = convert_to_gensim_dict(X)

        # Extract topic words from the model
        topics_words = []
        for topic_idx, topic in enumerate(lda_model.components_):
            topic_words = [gensim_dict[int(i)] for i in topic.argsort()[:-11:-1]]
            topics_words.append(topic_words)

        # Convert X to a list of documents
        documents_list = [" ".join(map(str, row)) for row in X.astype(int)]

        # Calculate coherence score
        coherence_model_lda = CoherenceModel(topics=topics_words, texts=documents_list, dictionary=gensim_dict, coherence='c_v')
        coherence_score = coherence_model_lda.get_coherence()

        print(f"Number of Topics: {num_topics}, Coherence Score: {coherence_score}")
        coherence_scores.append(coherence_score)

    return coherence_scores



# Function to find the elbow point and return the optimal number of topics
def find_optimal_num_topics(scores, min_topics):
    # Calculate the rate of change in perplexity scores
    rate_of_change = [scores[i] - scores[i - 1] for i in range(1, len(scores))]

    # Identify the index where the rate of change decreases significantly
    elbow_point_index = rate_of_change.index(min(rate_of_change)) + 1  # Add 1 to get the index of the elbow point
    
    # Return the optimal number of topics
    optimal_num_topics = min_topics + elbow_point_index
    return optimal_num_topics


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

    # Set the maximum number of topics to consider
    min_topics = 5
    max_topics = 20

    coherence_scores = calculate_coherence_scores(tfidf_matrix_scaled, min_topics, max_topics)

    # Plot coherence scores
    topics_range = range(min_topics, max_topics + 1)
    plt.plot(topics_range, coherence_scores, marker='o')
    plt.title('Coherence Scores for Different Number of Topics')
    plt.xlabel('Number of Topics')
    plt.ylabel('Coherence Score')
    plt.show()

    print(coherence_scores)
    exit(0)

    # Calculate perplexity scores for a range of topics
    perplexity_scores = calculate_perplexity_scores(tfidf_matrix_scaled, min_topics, max_topics)
    print(perplexity_scores)

    # Plot the perplexity scores
    plt.plot(range(min_topics, max_topics + 1), perplexity_scores, marker='o')
    plt.title('Perplexity Scores for Different Number of Topics (k)')
    plt.xlabel('Number of Topics (k)')
    plt.ylabel('Perplexity Score')
    plt.savefig('perplexity_plot.png')

    # Find the optimal number of topics (k) based on the elbow point
    optimal_num_topics = find_optimal_num_topics(perplexity_scores, min_topics)
    print(f"Optimal Number of Topics (k): {optimal_num_topics}")
    
    coherence_scores = calculate_coherence_scores(tfidf_matrix_scaled, min_topics, max_topics)
    print(coherence_scores)
    # Plot coherence scores
    plt.plot(range(min_topics, max_topics + 1), coherence_scores, marker='o')
    plt.title('Coherence Scores for Different Number of Topics')
    plt.xlabel('Number of Topics')
    plt.ylabel('Coherence Score')
    plt.show()
    exit(0)


    # Fit LDA model with the chosen number of topics
    num_topics = 11  # Adjust as needed
    lda_model = LatentDirichletAllocation(n_components=num_topics, random_state=42)
    lda_topic_matrix = lda_model.fit_transform(tfidf_matrix_scaled)

    # Check the shape of your TF-IDF matrix
    num_documents, num_features = tfidf_matrix.shape
    print(f"Number of Documents: {num_documents}, Number of Features: {num_features}")

    # Get feature names from TF-IDF vectorizer
    feature_names = np.array(tfidf_vectorizer.get_feature_names_out())

    # Display the top terms for each topic in the grid
    fig, axes = plt.subplots(6, 3, figsize=(15, 15), sharex=True)  # Adjust the grid size based on your preference

    for topic_idx, ax in enumerate(axes.flatten()):
        if topic_idx < num_topics:
            top_terms_idx = lda_model.components_[topic_idx].argsort()[:-10 - 1:-1]  # Display top 10 terms
            top_terms = feature_names[top_terms_idx]
        
            ax.barh(range(len(top_terms)), lda_model.components_[topic_idx][top_terms_idx], align='center')
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

    '''
    # Private
    private_vocab = pd.read_csv(PRIVATE_VOCAB_FILE)
    private_vocab = clean_vocab(private_vocab, "arbeitgeber")
    private_vocab_reduced = private_vocab[["ID", "lemma"]]
    '''


