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
library(slam)

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
library(openxlsx)

library(purrr)
library(htmltools)

# --------------------------------- Settings --------------------------------- #
setwd("~/interamtdb/NLP")
options(scipen = 999)

# --------------------------------- Constants -------------------------------- #
SAMPLE_SIZE <- 500
WORDS_BY_EMPLOYER_MIN <- 1
LETTERS_PER_WORD_MIN <- 3
PUBLIC_ADS_FILE <- "public_ads.csv"
PRIVATE_ADS_FILE <- "private_ads.csv"
MIN_WORDS <- 4 # see word2vec min_count
MIN_WORD_BY_ID <- 50
IDF_MIN_PERC <- 0.1

# --------------------------------- Functions -------------------------------- #
sample_first_n_ids <- function(vocab) {
  unique_ids <- unique(vocab$ID)
  sampled_ids <- unique_ids[1:SAMPLE_SIZE]
  return(vocab[vocab$ID %in% sampled_ids,])
}

get_cleansed_vocab <- function(vocab, employer_column) {
  # Tag cleansing
  vocab <- na.omit(vocab)
  vocab$tag <- gsub("\\(.+\\)", "", vocab$tag)
  # Group by 'lemma' and 'tag', then summarize
  vocab <- vocab %>%
    group_by(ID) %>%
    mutate(words_in_id = n()) %>%
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
    mutate(n = n(),
           letters = nchar(lemma)) %>%
    filter(n > MIN_WORDS,
           letters >= LETTERS_PER_WORD_MIN)
  return(vocab)
}

similarity_to_distance <- function(similarity_matrix) {
  n <- nrow(similarity_matrix)
  distance_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      distance_matrix[i, j] <-
        round(1 - similarity_matrix[i, j], digits = 4)
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
  return(data.frame(
    height = dividers,
    branches = branches,
    avg_labels = avg_labels
  ))
}

normalizer <- function(v) {
  min_value <- min(v)
  max_value <- max(v)
  v_n <- ((v - min_value) / (max_value - min_value))
  return(v_n)
}

replace_with_cluster <- function(word, cluster_list) {
  for (i in seq_along(cluster_list)) {
    label <- first(cluster_list[[i]]$label)
    words = cluster_list[[i]]$words
    if (any(word %in% words)) {
      return(label)
    }
  }
  return(NA)
}

get_associated_words <- function(word) {
  # URL encode the word
  word <- URLencode(word)
  response <- GET(
    paste0(
      "https://www.openthesaurus.de/synonyme/search?q=",
      word,
      "&format=application/json"
    )
  )
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    synonyms <- lapply(content$synsets, function(x)
      x$terms)
    synonyms <- unlist(synonyms)
    synonyms <- synonyms[synonyms != URLdecode(word)]
    return(synonyms)
  } else {
    stop("API request failed")
  }
}

concatenate_words <- function(x) {
  if (length(x) > 0) {
    paste(x[1:length(x)], collapse = ", ")
  } else {
    NA
  }
}

synonyms_distance_matrix <- function(v) {
  set.seed(1234)
  w2v_model <-
    word2vec(
      x = v,
      type = "skip-gram",
      iter = 10,
    )
  word_embeddings_vec <- as.matrix(w2v_model)
  # Remove end-of-sentence token
  word_embeddings_vec <-
    word_embeddings_vec[rownames(word_embeddings_vec) != "</s>",]
  similarity_matrix_synonyms <-
    word2vec_similarity(word_embeddings_vec, word_embeddings_vec, type = "cosine")
  similarity_matrix_synonyms <-
    normalizer(similarity_matrix_synonyms)
  distance_matrix_synonyms <-
    similarity_to_distance(similarity_matrix_synonyms)
  return(order_matrix(distance_matrix_synonyms))
}

lcs_distance_matrix <- function(unique_words) {
  distance_matrix_phonetic <-
    stringdistmatrix(unique_words, unique_words, method = "lcs")
  distance_matrix_phonetic <- normalizer(distance_matrix_phonetic)
  rownames(distance_matrix_phonetic) <- unique_words
  colnames(distance_matrix_phonetic) <- unique_words
  return(order_matrix(distance_matrix_phonetic))
}

thesaurus_distance_matrix <- function(unique_words, group, load) {
  associated_words_list <- list()
  if (load == TRUE) {
    cat("Load", group, "thesaurus from local file...", "\n")
    associated_words_list <-
      readRDS(paste("thesaurus_", group, "_words.rds", sep = ""))
  } else {
    cat("Gather", group, "thesaurus from online source...", "\n")
    for (i in seq_along(unique_words)) {
      associated_words <- get_associated_words(unique_words[i])
      if (is.null(associated_words)) {
        associated_words <- list()
      }
      associated_words_list[[unique_words[i]]] <- associated_words
      Sys.sleep(1)
      cat(i, "of", length(unique_words), "\n")
    }
    saveRDS(associated_words_list,
            file = paste("thesaurus_", group, "_words.rds", sep = ""))
  }
  
  associated_words_list_cleansed <-
    vector(mode = "list", length = length(associated_words_list))
  names(associated_words_list_cleansed) <-
    names(associated_words_list)
  
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
  distance_matrix_thesaurus <-
    matrix(-1, nrow = num_words, ncol = num_words)
  
  for (i in seq_along(words)) {
    for (j in seq_along(words)) {
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
  return(order_matrix(distance_matrix_thesaurus))
}

add_matrices <- function(matrix1, matrix2, matrix3) {
  return(matrix1 + matrix2 + matrix3)
}

order_matrix <- function(m) {
  matrix_ordered <- m[order(rownames(m)), order(colnames(m))]
  return(matrix_ordered)
}

cluster_labels <- function(distance_matrix, h) {
  dend <- distance_matrix %>%
    dist() %>%
    hclust(method = "average") %>%
    as.dendrogram()
  
  bph <- branches_per_height(dend, k = 0.5)
  bph <- mutate(bph, agg = lag(branches) - branches)
  bph[1,]$agg <- 0
  bph$cum_agg <- cumsum(bph$agg)
  bph$cum_agg_n <- normalizer(bph$cum_agg)
  print(bph)
  
  dendlist <- cut(dend, h)
  
  my_labels <- list()
  for (d in dendlist$lower) {
    my_labels <- c(my_labels, list(d %>% labels()))
  }
  names(my_labels) <- paste("label", seq_along(my_labels), sep = "")
  
  cat(length(my_labels), "labels generated.\n")
  
  my_labels <- lapply(seq_along(my_labels), function(i) {
    data.frame(label = names(my_labels[i]), words = my_labels[[i]])
  })
  
  my_labels_df <- do.call(rbind, my_labels)
  return(my_labels_df)
}

cluster_tagging <-
  function(cleansed_dataframe, labels_df, group, h) {
    cleansed_dataframe_source <- cleansed_dataframe %>%
      ungroup() %>%
      select(ID, sentence_index, label) %>%
      group_by(ID, sentence_index) %>%
      reframe(sentence = paste(label, collapse = " ")) %>%
      ungroup() %>%
      select(ID, sentence) %>%
      group_by(ID) %>%
      reframe(text = paste(sentence, collapse = " ")) %>%
      rename(doc_id = ID)
    
    set.seed(1234)
    w2v_model <-
      word2vec(
        x = cleansed_dataframe_source$text,
        type = "skip-gram",
        iter = 200
      )
    word_embeddings_vec <- as.matrix(w2v_model)
    word_embeddings_vec <-
      word_embeddings_vec[rownames(word_embeddings_vec) != "</s>",]
    similarity_matrix <-
      word2vec_similarity(word_embeddings_vec, word_embeddings_vec, type = "cosine")
    similarity_matrix <- normalizer(similarity_matrix)
    distance_matrix <- similarity_to_distance(similarity_matrix)
    
    dend <- distance_matrix %>%
      dist() %>%
      hclust(method = "average") %>%
      as.dendrogram()
    
    bph <- branches_per_height(dend, k = 0.2)
    bph <- mutate(bph, agg = lag(branches) - branches)
    bph[1,]$agg <- 0
    bph$cum_agg <- cumsum(bph$agg)
    bph$cum_agg_n <- normalizer(bph$cum_agg)
    print(bph)
    
    dendlist <- cut(dend, h)
    
    clusters <- list()
    for (d in dendlist$lower) {
      clusters <- c(clusters, list(d %>% labels()))
    }
    
    clusters <- lapply(seq_along(clusters), function(i) {
      data.frame(cluster = i, label = clusters[[i]])
    })
    
    cat(length(clusters), "clusters generated.\n")
    
    clusters_df <- do.call(rbind, clusters)
    
    clusters_df <- clusters_df %>%
      left_join(labels_df)
    
    export <- clusters_df %>%
      select(cluster, words) %>%
      group_by(cluster) %>%
      mutate(row_id = row_number()) %>%
      pivot_wider(names_from = cluster,
                  values_from = words,
                  values_fn = list) %>%
      select(-row_id)
    
    write.xlsx(export, paste("cluster_", group, ".xlsx", sep = ""), keepNA = TRUE)
    return(clusters_df)
  }

create_html_output <- function(df, group, employer_column, n) {
  if (n > length(unique(df$ID))) {
    print(
      "The number of unique IDs is less than the specified value of 'n'. Please provide a smaller value of 'n'."
    )
  }
  
  cluster_names <-
    c(sort(as.character(unique(df$cluster_name[df$cluster_name != "NA"]))), "NA")
  
  cluster_colors <-
    setNames(alpha(hcl(
      seq(15, 375, length.out = n_distinct(cluster_names)), 80, 80
    ), 1), unique(cluster_names))
  
  html_head <-
    "<!DOCTYPE html>\n<html>\n<head>\n<style>\nbody {\n  font-family: 'Arial', sans-serif;\n}\n</style>\n</head>\n\n<body>\n"
  
  html_foot <- "</body>\n</html>"
  
  alpha_name <- paste("alpha_", group, sep = "")
  
  generate_document_html <- function(data) {
    data$word[data$word == "\\n"] <- "<br>"
    data$word[data$word == "\\t"] <- "&nbsp;"
    
    document_html <- paste(
      paste(
        "<h3>ID: ",
        data$ID[1],
        ", ",
        employer_column,
        ": ",
        data[[employer_column]][1],
        "</h3>",
        sep = ""
      ),
      paste(lapply(seq_len(nrow(
        data
      )), function(i) {
        ifelse(
          is.na(data$cluster_name[i]),
          paste(
            "<span style='background-color: transparent;' title='Cluster: NA'>",
            data$word[i],
            "</span>",
            sep = ""
          ),
          paste(
            paste(
              "<span style='background-color:",
              cluster_colors[data$cluster_name[i]],
              "; opacity:", data[[alpha_name]][i],
              ";",
              ifelse(data[[alpha_name]][i] >= 0.8, "font-weight: bold;", ""),
              "' title='Cluster: ",
              data$cluster_name[i],
              "'>",
              data$word[i],
              "</span>",
              sep = ""
            )
          )
        )
      }),
      collapse = " "),
      "<hr>"
    )
    
    return(document_html)
  }
  
  legend_html <- paste("<h3>Legend</h3>",
                       paste(lapply(cluster_names, function(cluster) {
                         if (cluster == "NA") {
                           paste("Cluster:",
                                 "<span style='background-color: transparent;'>NA</span>")
                         } else {
                           paste(
                             "Cluster:",
                             "<span style='background-color:",
                             cluster_colors[cluster],
                             ";'>",
                             cluster,
                             "</span>"
                           )
                         }
                       }),
                       collapse = "<br>"))
  
  document_html_list <- lapply(unique(df$ID)[1:n], function(id) {
    document_data <- df[df$ID == id, ]
    generate_document_html(document_data)
  })
  
  final_html <-
    c(html_head, legend_html, document_html_list, html_foot)
  
  final_html_string <- paste(final_html, collapse = "\n")
  
  output_file <- paste("html_output_", group, ".html", sep = "")
  writeLines(final_html_string, output_file)
}

log_likelihood <- function(n_public, n_private, E_public, E_private) {
  result <- 2 * ((n_public * ifelse(n_public == 0 | E_public == 0, 0, log(n_public/E_public))) + 
                   (n_private * ifelse(n_private == 0 | E_private == 0, 0, log(n_private/E_private))))
  return(result)
}

log_ratio <- function(norm_public, public_total_words, private_total_words, norm_private) {
  zero_frequency <- 0.5
  result <- log2((ifelse(norm_public == 0, zero_frequency/public_total_words, norm_public)) / 
                  (ifelse(norm_private == 0, zero_frequency/private_total_words, norm_private)))
  return(result)
}

# ------------------------ Load datasets and sampling ------------------------ #
# Public
public_ads_raw <- read.csv(PUBLIC_ADS_FILE, na.strings = c(""))
public_ads_sample_raw <- sample_first_n_ids(public_ads_raw)
rm(public_ads_raw)
public_ads_cleansed <-
  get_cleansed_vocab(public_ads_sample_raw, "Behörde")

unique_words_public <- public_ads_cleansed %>%
  select(lemma) %>%
  group_by(lemma) %>%
  summarise(n = n())

public_total_words = sum(unique_words_public$n)

public_ads_text_cleansed <- public_ads_cleansed %>%
  group_by(ID) %>%
  summarize(lemma = paste(lemma, collapse = " "))

# Private
private_ads_raw <- read.csv(PRIVATE_ADS_FILE, na.strings = c(""))
private_ads_sample_raw <- sample_first_n_ids(private_ads_raw)
rm(private_ads_raw)
private_ads_cleansed <-
  get_cleansed_vocab(private_ads_sample_raw, "arbeitgeber")

unique_words_private <- private_ads_cleansed %>%
  select(lemma) %>%
  group_by(lemma) %>%
  summarise(n = n())

private_total_words = sum(unique_words_private$n)

private_ads_text_cleansed <-
  private_ads_cleansed[c("ID", "lemma")] %>%
  group_by(ID) %>%
  summarize(lemma = paste(lemma, collapse = " "))

# -------------------------------- Zipf's law -------------------------------- #
# Public
temp_public <- public_ads_cleansed %>%
  select(lemma, tag, document_frequency) %>%
  unique() %>%
  arrange(desc(document_frequency)) %>%
  mutate(vocab = "Public") %>%
  ungroup()
temp_public$rank <- seq_len(nrow(temp_public))
temp_public <-
  temp_public %>% select(rank, document_frequency, vocab)

# Private
temp_private <- private_ads_cleansed %>%
  select(lemma, tag, document_frequency) %>%
  unique() %>%
  arrange(desc(document_frequency)) %>%
  mutate(vocab = "Private") %>%
  ungroup()
temp_private$rank <- seq_len(nrow(temp_private))
temp_private <-
  temp_private %>% select(rank, document_frequency, vocab)

# Plotting Zipf's law (see https://doi.org/10.3758/s13423-014-0585-6)
rbind(temp_public, temp_private) %>%
  ggplot(aes(rank, document_frequency, color = vocab)) +
  geom_line(alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()

rm(temp_public, temp_private)

# ------------------------ Generate distance measures ------------------------ #
# Synonyms, semantic relationships on word level
public_distance_matrix_synonyms <-
  synonyms_distance_matrix(public_ads_text_cleansed$lemma)
private_distance_matrix_synonyms <-
  synonyms_distance_matrix(private_ads_text_cleansed$lemma)

# Phonetic relationship (Longest Common Substring Distance)
public_distance_matrix_phonetic <-
  lcs_distance_matrix(unique_words_public$lemma)
private_distance_matrix_phonetic <-
  lcs_distance_matrix(unique_words_private$lemma)

# Thesaurus relationship
public_distance_matrix_thesaurus <-
  thesaurus_distance_matrix(unique_words_public$lemma, group = "public", load = TRUE)
private_distance_matrix_thesaurus <-
  thesaurus_distance_matrix(unique_words_private$lemma, group = "private", load = TRUE)

# Addition of matrices, add weights in results getting better
public_combined_distance_matrix <- add_matrices(
  public_distance_matrix_synonyms,
  public_distance_matrix_phonetic,
  public_distance_matrix_thesaurus
)

private_combined_distance_matrix <- add_matrices(
  private_distance_matrix_synonyms,
  private_distance_matrix_phonetic,
  private_distance_matrix_thesaurus
)

# Label clustering, adjust h value based on coherent results
public_labels_df <-
  cluster_labels(public_combined_distance_matrix, 8.5)

private_labels_df <-
  cluster_labels(private_combined_distance_matrix, 6.3)

# --------- Label replacement and Cluster Extraction using Word2Vec ---------- #
public_ads_cleansed_labels <- public_ads_cleansed %>%
  left_join(public_labels_df, by = c('lemma' = 'words'))

private_ads_cleansed_labels <- private_ads_cleansed %>%
  left_join(private_labels_df, by = c('lemma' = 'words'))

public_cluster <-
  cluster_tagging(public_ads_cleansed_labels, public_labels_df, "public", 2.3)

private_cluster <-
  cluster_tagging(private_ads_cleansed_labels, private_labels_df, "private", 2)

# ---------------------------- Corpora comparison ---------------------------- #
# After manual inspection, assign names for clusters

public_cluster_names <- c("Requirements", "Tasks", "Conditions", "Company", "Requirements", "Company", "Application")
private_cluster_names <- c("Tasks", "Requirements", "Conditions", "Company", "Tasks", "Application", "Conditions")

public_cluster_names <- data.frame(
  cluster_nr = unique(public_cluster$cluster),
  cluster_name = public_cluster_names
)

private_cluster_names <- data.frame(
  cluster_nr = unique(private_cluster$cluster),
  cluster_name = private_cluster_names
)

public_cluster <- public_cluster %>%
  left_join(public_cluster_names, by = c('cluster' = 'cluster_nr'))

private_cluster <- private_cluster %>%
  left_join(private_cluster_names, by = c('cluster' = 'cluster_nr'))

public_ads_sample_raw_clusters <- public_ads_sample_raw %>%
  left_join(public_cluster, by = c('lemma' = 'words'))

private_ads_sample_raw_clusters <- private_ads_sample_raw %>%
  left_join(private_cluster, by = c('lemma' = 'words'))

# ------------------------------- Descriptives ------------------------------- #
# TODO

# ----------------------------- Word differences ----------------------------- #
joined_words <-
  full_join(
    unique_words_public,
    unique_words_private,
    by = c('lemma'),
    suffix = c("_public", "_private")
  )
joined_words[is.na(joined_words)] <- 0

joined_words <- joined_words %>%
  mutate(over_underuse = ifelse(n_public > n_private, 1,-1)) %>%
  mutate(E_public = public_total_words * (n_public + n_private) / (public_total_words + private_total_words)) %>%
  mutate(E_private = private_total_words * (n_public + n_private) / (public_total_words + private_total_words)) %>%
  mutate(norm_public = n_public / public_total_words) %>%
  mutate(norm_private = n_private / private_total_words) %>%
  mutate(Log_likelihood = log_likelihood(n_public, n_private, E_public, E_private)) %>%
  mutate(Log_ratio = log_ratio(norm_public, public_total_words, private_total_words, norm_private))

joined_words_non_zero <- joined_words[joined_words$n_public > 0 & joined_words$n_private > 0, ]

lower_quantil <- quantile(joined_words_non_zero$Log_ratio, 0.10)
upper_quantil <- quantile(joined_words_non_zero$Log_ratio, 0.90)
mean <- mean(joined_words_non_zero$Log_ratio)

ggplot(joined_words_non_zero, aes(x = Log_ratio)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "#69b3a2",
    color = "white",
    alpha = 0.9
  ) +
  geom_vline(xintercept = lower_quantil, linetype = "dashed") +
  geom_vline(xintercept = upper_quantil, linetype = "dashed") +
  geom_vline(xintercept = mean, color = "red") +
  geom_text(aes(
    x = mean,
    label = round(mean, digits = 2),
    y = 100
  ), color = "red") +
  theme_light() +
  xlab("Log ratio of the relative keyword frequencies (negative = private, positive = public)") +
  ylab("Frequency")

# Log likelihood
# See: https://ucrel.lancs.ac.uk/llwizard.html
# 95th percentile; 5% level; p < 0.05; critical value = 3.84
# 99th percentile; 1% level; p < 0.01; critical value = 6.63
# 99.9th percentile; 0.1% level; p < 0.001; critical value = 10.83
# 99.99th percentile; 0.01% level; p < 0.0001; critical value = 15.13

joined_words_99_99 <- joined_words[joined_words$Log_likelihood > 15.13, ]

# Show via alpha parameter, 0 = transparent, 1 = opaque
joined_words$alpha_public <- normalizer(joined_words$Log_ratio)
joined_words$alpha_private <- normalizer(joined_words$Log_ratio * -1)
# TODO: Decide whether significance level matters or how it can be depicted

public_ads_sample_raw_clusters <- public_ads_sample_raw_clusters %>%
  left_join(
    joined_words %>%
      select(lemma, alpha_public)
  )

private_ads_sample_raw_clusters <- private_ads_sample_raw_clusters %>%
  left_join(
    joined_words %>%
      select(lemma, alpha_private)
  )

# ------------------------------- HMTL Output -------------------------------- #
create_html_output(public_ads_sample_raw_clusters, "public", "Behörde", 10)

create_html_output(private_ads_sample_raw_clusters, "private", "arbeitgeber", 10)

