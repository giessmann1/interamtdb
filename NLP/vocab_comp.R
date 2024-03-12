# ---------------------------------------------------------------------------- #
#           Vocabulary comparsion between public and private job ads           #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

# ---------------------------------- Imports --------------------------------- #
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

library(torch)
library(topicmodels.etm)
library(word2vec)
library(udpipe)
library(textplot)
library(uwot)
library(ggrepel)
library(hrbrthemes)
library(ggalt)
library(data.table)
library(stringr)

library(topicmodels)
library(NLP)
library(tm)

library(proxy)
library(reshape2)
library(cluster)
library(dendextend)
library(ggraph)
library(igraph)
library(colormap)
library(ape)
library(circlize)
library(ggdendro)
library(stringdist)

library(httr)
library(xml2)

# --------------------------------- Settings --------------------------------- #
setwd("~/interamtdb/NLP")
options(scipen = 999)

# --------------------------------- Constants -------------------------------- #
SAMPLE_SIZE <- 200
WORDS_BY_EMPLOYER_MIN <- 1
PUBLIC_VOCAB_FILE <- "public_vocab.csv"
PRIVATE_VOCAB_FILE <- "private_vocab.csv"
MIN_WORDS <- 4 # see word2vec min_count
MIN_WORD_BY_ID <- 50
IDF_MIN_PERC <- 0.1

# --------------------------------- Functions -------------------------------- #
sample_first_n_ids <- function(vocab) {
  vocab <- na.omit(vocab)
  unique_ids <- unique(vocab$ID)
  sampled_ids <- unique_ids[1:SAMPLE_SIZE]
  return(vocab[vocab$ID %in% sampled_ids, ])
}

get_cleansed_vocab <- function(vocab, employer_column) {
  # Tag cleansing
  vocab$tag <- gsub("\\(.+\\)", "", vocab$tag)
  # Group by 'lemma' and 'tag', then summarize
  vocab <- vocab %>%
    group_by(ID) %>%
    mutate(
      words_in_id = n()
    ) %>%
    ungroup() %>%
    filter(words_in_id > MIN_WORD_BY_ID) %>%
    group_by(lemma) %>%
    mutate(
      document_frequency = n_distinct(ID),
       # Curly double braces doesn't work for me.
      word_by_employers = n_distinct(.data[[employer_column]]),
      idf = log2(SAMPLE_SIZE / document_frequency)
    ) %>%
    ungroup() %>%
    filter(word_by_employers > WORDS_BY_EMPLOYER_MIN) %>%
    # Saturation filter, Risk of filtering out dominant sector-specific terms
    filter(idf > quantile(idf, IDF_MIN_PERC)) %>%
    group_by(lemma) %>%
    mutate(
      n = n()
    ) %>%
    filter(n > MIN_WORDS) %>%
    ungroup()
  return(vocab)
}

similarity_to_distance <- function(similarity_matrix) {
  n <- nrow(similarity_matrix)
  distance_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      distance_matrix[i, j] <- round(1 - similarity_matrix[i, j], digits = 4)
    }
  }
  rownames(distance_matrix) <- rownames(similarity_matrix)
  colnames(distance_matrix) <- colnames(similarity_matrix)
  return(distance_matrix)
}

get_dividers <- function(n, k) {
  dividers <- seq(k, n, by = k)
  if (dividers[length(dividers)] != n) {
    dividers[length(dividers) + 1] <- n
  }
  return(dividers)
}

branches_per_height <- function(dend, k = 1) {
  n <- get_branches_heights(dend, decreasing = TRUE)[1]
  dividers <- get_dividers(n, k)
  branches <- c()
  avg_labels <- c()
  for (i in dividers) {
    dendlist <- cut(dend, h = i)$lower
    dendlist_l <- length(dendlist)
    branches <- append(branches, dendlist_l)
    sum_labels <- 0
    for (d in dendlist) {
      sum_labels = sum_labels + length(d %>% labels())
    }
    avg_labels <- append(avg_labels, sum_labels / dendlist_l)
  }
  return(
    data.frame(height = dividers, branches = branches, avg_labels = avg_labels))
}

normalizer <- function(v) {
  min_value <- min(v)
  max_value <- max(v)
  v_n <- ((v - min_value) / (max_value - min_value))
  return(v_n)
}

replace_with_cluster <- function(word, cluster_list) {
  for (i in seq_along(cluster_list)) {
    name <- names(cluster_list[i])
    cluster = cluster_list[[i]]
    if (any(word %in% cluster)) {
      return(name)
    }
  }
  return(NA)
}

get_associated_words <- function(word) {
  # URL encode the word
  word <- URLencode(word)
  response <- GET(
    paste0("https://www.openthesaurus.de/synonyme/search?q=", word, "&format=application/json"))
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    synonyms <- lapply(content$synsets, function(x) x$terms)
    synonyms <- unlist(synonyms)
    synonyms <- synonyms[synonyms != URLdecode(word)]
    return(synonyms)
  } else {
    stop("API request failed")
  }
}

# ------------------------ Load datasets and sampling ------------------------ #
# Public
public_vocab <- read.csv(PUBLIC_VOCAB_FILE, na.strings=c(""))
public_vocab_sample <- sample_first_n_ids(public_vocab)
public_vocab_cleansed <- get_cleansed_vocab(public_vocab_sample, "Behörde")

unique_words_public <- unique(public_vocab_cleansed$lemma)

public_ads <- public_vocab_cleansed %>%
  group_by(ID) %>%
  summarize(
    lemma = paste(lemma, collapse = " ")
  )

# Private
private_vocab <- read.csv(PRIVATE_VOCAB_FILE, na.strings=c(""))
private_vocab_sample <- sample_first_n_ids(private_vocab)
private_vocab_cleansed <- 
  get_cleansed_vocab(private_vocab_sample, "arbeitgeber")

unique_words_private <- unique(private_vocab_cleansed$lemma)

private_ads <- private_vocab_cleansed[c("ID", "lemma")] %>%
  group_by(ID) %>%
  summarize(lemma = paste(lemma, collapse = " "))

# -------------------------------- Zipf's law -------------------------------- #
# Public
temp_public <- public_vocab_cleansed %>%
  select(lemma, tag, document_frequency) %>%
  unique() %>%
  arrange(desc(document_frequency)) %>%
  mutate(vocab = "Public") %>%
  ungroup()
temp_public$rank <- seq_len(nrow(temp_public))
temp_public <- temp_public %>% select(rank, document_frequency, vocab)

# Private
temp_private <- private_vocab_cleansed %>%
  select(lemma, tag, document_frequency) %>%
  unique() %>%
  arrange(desc(document_frequency)) %>%
  mutate(vocab = "Private") %>%
  ungroup()
temp_private$rank <- seq_len(nrow(temp_private))
temp_private <- temp_private %>% select(rank, document_frequency, vocab)

# Plotting Zipf's law (see https://doi.org/10.3758/s13423-014-0585-6)
rbind(temp_public, temp_private) %>%
  ggplot(aes(rank, document_frequency, color = vocab)) +
  geom_line(alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

rm(temp_public, temp_private)

# ------------------------ Generate distance measures ------------------------ #
# Semantic relationships
w2v_model <- word2vec(x = public_ads$lemma, type = "skip-gram", iter = 10, threads = 4)
word_embeddings_vec <- as.matrix(w2v_model)
# Remove end-of-sentence token
word_embeddings_vec <- word_embeddings_vec[rownames(word_embeddings_vec) != "</s>", ]

similarity_matrix_semantic <- word2vec_similarity(word_embeddings_vec, word_embeddings_vec, type = "cosine")
similarity_matrix_semantic <- normalizer(similarity_matrix_semantic)
distance_matrix_semantic <- similarity_to_distance(similarity_matrix_semantic)

# Phonetic relationship
# Levenshtein Distance
distance_matrix_phonetic <- stringdistmatrix(unique_words_public, unique_words_public, method = "lcs")
distance_matrix_phonetic <- normalizer(similarity_matrix)
rownames(distance_matrix_phonetic) <- unique_words_public
colnames(distance_matrix_phonetic) <- unique_words_public

# Thesaurus relationship
associated_words_list <- list()

for (i in seq_along(unique_words_public)) {
  associated_words <- get_associated_words(unique_words_public[i])
  if (is.null(associated_words)) {associated_words <- list()}
  associated_words_list[[unique_words_public[i]]] <- associated_words
  Sys.sleep(1)
}
saveRDS(associated_words_list, file = "thesaurus_raw.rds")

associated_words_list_cleansed <-
  vector(mode = "list", length = length(associated_words_list))
names(associated_words_list_cleansed) <- names(associated_words_list)

for (i in seq_along(associated_words_list_cleansed)) {
  t <- associated_words_list[[i]]
  t_new <- list()
  if (length(t) > 0) {
    for (j in seq_along(t)) {
      if (names(t[j]) == "term") {
        tj_new <- str_replace_all(t[j], "\\(.*?\\)", "")
        tj_new <- gsub("[^a-zA-ZÄÄöÖÜüß ]", "", tj_new)
        tj_new <- trimws(tj_new)
        if (length(unlist(strsplit(tj_new, split = " "))) > 1) {
          next
        } else {
          tj_new <- tolower(tj_new)
          t_new <- c(t_new, tj_new)
        }
      }
    }
    t_new <- unique(t_new)
  }
  associated_words_list_cleansed[[i]] <- t_new
}

words <- names(associated_words_list_cleansed)
num_words <- length(words)

# Initialize an empty distance matrix with -1 indicating an undefined relationship
distance_matrix_thesaurus <- matrix(-1, nrow = num_words, ncol = num_words)

for (i in seq_along(num_words)) {
  for (j in seq_along(num_words)) {
    # Check if the distance is undefined (-1)
    if (distance_matrix_thesaurus[i, j] == -1) {
      # Same words
      if (i == j) {
        distance_matrix_thesaurus[i, j] <- 0
        distance_matrix_thesaurus[j, i] <- 0
      } else if (words[i] %in% unlist(associated_words_list_cleansed[[j]]) ||
          words[j] %in% unlist(associated_words_list_cleansed[[i]])) {
        # Set distance to 0 if there's a connection in either direction
        distance_matrix_thesaurus[i, j] <- 0
        # Set distance to 0 for the other direction as well
        distance_matrix_thesaurus[j, i] <- 0
      } else {
        distance_matrix_thesaurus[i, j] <- 1
        distance_matrix_thesaurus[j, i] <- 1
      }
    }
  }
}

colnames(distance_matrix_thesaurus) <- words
rownames(distance_matrix_thesaurus) <- words
rm(words, num_words)

# Addition of matrices
combined_distance_matrix <- distance_matrix_semantic + distance_matrix_phonetic + distance_matrix_thesaurus

dend <- combined_distance_matrix %>%
  dist() %>%
  hclust(method = "average") %>%
  as.dendrogram()

bph <- branches_per_height(dend, k = 0.5)
bph <- mutate(bph, agg = lag(branches) - branches)
bph[1, ]$agg <- 0
bph$cum_agg <- cumsum(bph$agg)
bph$cum_agg_n <- normalizer(bph$cum_agg)

ggplot(bph, aes(x = height)) +
  geom_line(aes(y = branches)) +
  geom_line(aes(y = avg_labels*2)) +
  geom_vline(xintercept = 7.5, color = "red") +
  scale_y_continuous(
    name = "Cummulative amount of branches",
    sec.axis = sec_axis(trans = ~./2, name = "Average labels per branch")
  )

dendlist <- cut(dend, h = 7.5)

tm_labels <- list()
for (d in dendlist$lower){
  tm_labels <- c(tm_labels, list(d %>% labels()))
}
names(tm_labels) <- paste("label", seq_along(tm_labels), sep = "")

# ------------------- Label replacement and topic modeling ------------------- #
public_vocab_cleansed <- public_vocab_cleansed %>%
  rowwise() %>%
  mutate(tm_label = replace_with_cluster(lemma, tm_labels))


'''
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
'''

### LDA Topic modeling with VEM algorithm,
# but with searchK option (stm package)
# FREX weighting
# gamma values
# topicCorr


####### ETM with word embeddings
w2v <- word2vec(x = public_ads$lemma, dim = 25, type = "skip-gram", iter = 10, min_count = 5, threads = 2)
word_embeddings <- extract_vectors(w2v, words = colnames(w2v$words), norm = TRUE)

# predict(w2v, newdata = c("datenschutz"), type = "nearest", top_n = 5)

dtm <- strsplit.data.frame(public_ads, group = "ID", term = "lemma", split = " ") # Easier
dtm <- document_term_frequencies(dtm) # Easier
dtm <- document_term_matrix(dtm)
#dtm <- dtm_remove_tfidf(dtm, prob = 0.50)
vocab <- intersect(rownames(embeddings), colnames(dtm))
embeddings <- dtm_conform(embeddings, rows = vocab)
dtm <- dtm_conform(dtm, columns = vocab)

set.seed(1234)
torch_manual_seed(4321)
model <- ETM(k = 20, dim = 100, embeddings = embeddings)
optimizer <- optim_adam(params = model$parameters, lr = 0.005, weight_decay = 0.0000012)
loss <- model$fit(data = dtm, optimizer = optimizer, epoch = 20, batch_size = 1000)
plot(model, type = "loss")

terminology <- predict(model, type = "terms", top_n = 15)
terminology
terminology  <- rbindlist(terminology, idcol = "cluster")

centers    <- as.matrix(model, type = "embedding", which = "topics")
embeddings <- as.matrix(model, type = "embedding", which = "words")
manifold   <- umap(embeddings, 
                   n_components = 2, metric = "cosine", n_neighbors = 15, fast_sgd = TRUE, 
                   n_threads = 2, ret_model = TRUE, verbose = TRUE)
centers    <- umap_transform(X = centers, model = manifold)
words      <- manifold$embedding


df           <- list(words   = merge(x = terminology, 
                                     y = data.frame(x = words[, 1], y = words[, 2], term = rownames(embeddings)), 
                                     by = "term"), 
                     centers = data.frame(x = centers[, 1], y = centers[, 2], 
                                          term = paste("Topic-", seq_len(nrow(centers)), sep = ""), 
                                          cluster = seq_len(nrow(centers))))
df           <- rbindlist(df, use.names = TRUE, fill = TRUE, idcol = "type")
df           <- df[, weight := ifelse(is.na(beta), 0.8, beta / max(beta, na.rm = TRUE)), by = list(cluster)]

textplot_embedding_2d(df, title = "ETM topics", subtitle = "embedded in 2D using UMAP", encircle = FALSE, points = FALSE)
textplot_embedding_2d(df, title = "ETM topics", subtitle = "embedded in 2D using UMAP", encircle = TRUE, points = TRUE)

##### STM with categorical covariate 'sector'



# Private

# ---------------------------- Corpora comparison ---------------------------- #
joined_words <- full_join(public_words, private_words,by = c('lemma', 'tag'), suffix = c("_public", "_private"))
joined_words[is.na(joined_words)] <- 0

joined_words <- joined_words %>%
  mutate(over_underuse = ifelse(O_public > O_private, 1, -1)) %>%
  mutate(E_public = public_total_words * (n_public + n_private)/(public_total_words + private_total_words)) %>%
  mutate(E_private = private_total_words * (n_public + n_private)/(public_total_words + private_total_words)) %>%
  mutate(Log_likelihood = 2*((n_public*log(n_public / E_public)) + (n_private*log(n_private / E_private)))) %>%
  mutate(Log_ratio = log2(O_public / O_private))

lower_quantil <- quantile(joined_words$Log_ratio, 0.10)
upper_quantil <- quantile(joined_words$Log_ratio, 0.90)
mean <- mean(joined_words$Log_ratio)

ggplot(joined_words, aes(x = Log_ratio)) +
  geom_histogram(binwidth=0.5, fill="#69b3a2", color="white", alpha=0.9) +
  geom_vline(xintercept = lower_quantil, linetype = "dashed") +
  geom_vline(xintercept = upper_quantil, linetype = "dashed") +
  geom_vline(xintercept = mean, color="red") +
  geom_text(aes(x=mean, label=round(mean, digits = 2), y=100), color="red") +
  theme_light() +
  xlab("Log ratio of the relative keyword frequencies (negative = private, positive = public)") +
  ylab("Frequency")

# Log likelihood

# See: https://ucrel.lancs.ac.uk/llwizard.html
# 95th percentile; 5% level; p < 0.05; critical value = 3.84
# 99th percentile; 1% level; p < 0.01; critical value = 6.63
# 99.9th percentile; 0.1% level; p < 0.001; critical value = 10.83
# 99.99th percentile; 0.01% level; p < 0.0001; critical value = 15.13

PPDs <- subset(joined_words, Log_likelihood > 15.13 & over_underuse == 1)
PPDs_adj <- PPDs[grepl("ADJ", PPDs$tag), ]
PPDs_adj <- PPDs_adj[order(PPDs_adj$Log_ratio, decreasing = TRUE), ] %>% top_n(100)

PPDs_private <- subset(joined_words, Log_likelihood > 15.13 & over_underuse == -1)
PPDs_private_adj <- PPDs_private[grepl("ADJ", PPDs_private$tag), ]
PPDs_private_adj <- PPDs_private_adj[order(PPDs_private_adj$Log_ratio, decreasing = TRUE), ] %>% top_n(100)

set.seed(1234)
wordcloud(words = PPDs_adj$lemma, freq = abs(PPDs_adj$Log_ratio)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

set.seed(1234)
wordcloud(words = PPDs_private_adj$lemma, freq = abs(PPDs_private_adj$Log_ratio)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

 