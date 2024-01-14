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
library(ggalt)
library(data.table)

# --------------------------------- Settings --------------------------------- #
setwd("~/interamtdb/NLP")
options(scipen = 10000)

# --------------------------------- Constants -------------------------------- #
SAMPLE_SIZE <- 5000
WORDS_BY_EMPLOYER_MIN <- 1
PUBLIC_VOCAB_FILE <- "public_vocab.csv"
PRIVATE_VOCAB_FILE <- "private_vocab.csv"

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
    group_by(lemma, tag) %>%
    mutate(
      n = n(),
      document_frequency = n_distinct(ID),
      word_by_employers = n_distinct(.data[[employer_column]]), # Curly double braces doesn't work for me.
      idf = log2(SAMPLE_SIZE / document_frequency)
    ) %>%
    filter(word_by_employers > WORDS_BY_EMPLOYER_MIN) %>%
    select(-word_by_employers)
  
  return(vocab)
}

# ------------------------ Load datasets and sampling ------------------------ #
# Public
public_vocab <- read.csv(PUBLIC_VOCAB_FILE, na.strings=c(""))
public_vocab_sample <- sample_first_n_ids(public_vocab)
public_vocab_cleansed <- get_cleansed_vocab(public_vocab_sample, "Behörde")

public_ads <- public_vocab_cleansed[c("ID", "lemma")] %>%
  group_by(ID) %>%
  summarize(lemma = paste(lemma, collapse = ' '))

# Private
private_vocab <- read.csv(PRIVATE_VOCAB_FILE, na.strings=c(""))
private_vocab_sample <- sample_first_n_ids(private_vocab)
private_vocab_cleansed <- get_cleansed_vocab(private_vocab_sample, "arbeitgeber")

private_ads <- private_vocab_cleansed[c("ID", "lemma")] %>%
  group_by(ID) %>%
  summarize(lemma = paste(lemma, collapse = ' '))

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

# ---------------------------- Corpora generation ---------------------------- #
# Public
w2v <- word2vec(x = public_ads$lemma, dim = 25, type = "skip-gram", iter = 10, min_count = 5, threads = 2)
embeddings <- as.matrix(w2v)

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

 