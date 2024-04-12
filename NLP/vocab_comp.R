# ---------------------------------------------------------------------------- #
#           Vocabulary comparison between public and private job ads           #
#                                                                              #
#                     (c) Nico Gießmann, MA thesis, 2023-24                    #
# ---------------------------------------------------------------------------- #

# ---------------------------------- Imports --------------------------------- #
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(word2vec)
library(udpipe)
library(textplot)
library(uwot)
library(ggrepel)
library(stringr)
library(proxy)
library(reshape2)
library(cluster)
library(dendextend)
library(ggdendro)
library(stringdist)
library(httr)
library(xml2)
library(openxlsx)
library(purrr)
library(htmltools)

# --------------------------------- Settings --------------------------------- #
setwd("~/interamtdb/NLP")
options(scipen=999)

# --------------------------------- Constants -------------------------------- #
SAMPLE_SIZE <- 5000
WORDS_BY_EMPLOYER_MIN <- 2
LETTERS_PER_WORD_MIN <- 3
PUBLIC_ADS_FILE <- "input/public_ads.csv"
PRIVATE_ADS_FILE <- "input/private_ads.csv"
MIN_WORDS <- 4 # see word2vec min_count
MIN_WORD_BY_ID <- 100
IDF_MIN_PERC <- 0.1
WORDS_BLACKLIST_FILE <- "input/words_blacklist.csv"
WORDS_WHITELIST_FILE <- "input/words_whitelist.csv"

# --------------------------------- Functions -------------------------------- #
english_exclusive_words <- function() {
  # Dictionary reference:
  # D. Goldhahn, T. Eckart & U. Quasthoff: Building Large Monolingual Dictionaries at the Leipzig Corpora Collection: From 100 to 200 Languages.
  # In: Proceedings of the 8th International Language Resources and Evaluation (LREC'12), 2012
  english_words <- read.csv("input/eng_news_2023_30K-words.txt", header = FALSE, sep = "\t")
  english_words <- english_words %>%
    mutate(V2 = str_replace_all(tolower(V2), "[^a-zäöüÄÖÜß]", "")) %>%
    filter(nchar(V2) > 1) %>%
    unique()
  english_words <- english_words$V2
  
  german_words <- read.csv("input/deu_news_2023_30K-words.txt", header = FALSE, sep = "\t")
  german_words <- german_words %>%
    mutate(V2 = str_replace_all(tolower(V2), "[^a-zäöüÄÖÜß]", "")) %>%
    filter(nchar(V2) > 1) %>%
    unique()
  german_words <- german_words$V2
  
  return(setdiff(english_words, german_words))
}
  
sample_first_n_ids <- function(vocab) {
  unique_ids <- unique(vocab$ID)
  sampled_ids <- unique_ids[1:min(SAMPLE_SIZE, length(unique_ids))]
  sampled_vocab <- vocab[vocab$ID %in% sampled_ids,]
  sampled_vocab$tag <- gsub("\\(.+\\)", "", sampled_vocab$tag)
  # Remove blacklist words
  blacklist <- as.vector(read.csv(WORDS_BLACKLIST_FILE, header = FALSE))$V1
  sampled_vocab[sampled_vocab$lemma %in% blacklist, ]$lemma <- NA
  # English words, this should exclude typical English words,
  # without influencing technical terms (whitelist)
  whitelist <- as.vector(read.csv(WORDS_WHITELIST_FILE, header = FALSE))$V1
  excl_eng_words <- setdiff(english_exclusive_words(), whitelist)
  sampled_vocab[sampled_vocab$lemma %in% excl_eng_words, ]$lemma <- NA

  return(sampled_vocab)
}

get_cleansed_vocab <- function(vocab, employer_column) {
  # Tag cleansing
  vocab <- na.omit(vocab)
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
    word2vec(x = v,
             type = "skip-gram",
             iter = 10, )
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
      readRDS(paste("input/thesaurus_", group, "_words.rds", sep = ""))
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
            file = paste("input/thesaurus_", group, "_words.rds", sep = ""))
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

cluster_labels_dend <- function(distance_matrix) {
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
  
  return(dend)
}

cluster_labels_df <- function(dend, h) {
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

cluster_tagging_dend <- function(cleansed_dataframe, labels_df) {
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
      word2vec(x = cleansed_dataframe_source$text,
               type = "skip-gram",
               iter = 200)
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
    
    return(dend)
}
    
cluster_tagging_df <- function(dend, labels_df, group, h) {
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
    
    write.xlsx(export, paste("output/", group, "_cluster", ".xlsx", sep = ""), keepNA = TRUE)
    # TODO: Add csv output, remove write from code
    return(clusters_df)
  }

whitespace_after_needed <- function(word, word_after) {
  if (word_after %in% c("\\", "\\n", "<br>", NA) | word %in% c("\\", "\\n", "<br>")) {
    return(FALSE)
  }
  else if (substr(word_after, 1, 1) %in% c(".", ",", ";", ":", "!", "?", ")", "]", "-", "/") | substr(word, nchar(word), nchar(word)) %in% c("(", "[", "-", "/")) {
    return(FALSE)
  }
  
  return(TRUE)
}

replace_german_special_letters <- function(word) {
  replacements <- list(
    "Ä" = "&Auml;",
    "ä" = "&auml;",
    "Ö" = "&Ouml;",
    "ö" = "&ouml;",
    "Ü" = "&Uuml;",
    "ü" = "&uuml;",
    "ß" = "&szlig;",
    "É" = "&Eacute;",
    "é" = "&eacute;",
    "«" = "&laquo;",
    "»" = "&raquo;",
    "„" = "&bdquo;",
    "“" = "&ldquo;",
    "”" = "&rdquo;",
    "°" = "&deg;",
    "€" = "&euro;",
    "£" = "&pound;"
  )
  
  for (key in names(replacements)) {
    word <- gsub(key, replacements[[key]], word)
  }
  
  return(word)
}

create_html_output <- function(df, group, employer_column, n) {
  if (n > length(unique(df$ID))) {
    print(
      "The number of unique IDs is less than the specified value of 'n'. Please provide a smaller value of 'n'."
    )
  }
  
  cluster_names <- c(sort(as.character(unique(df$cluster_name[df$cluster_name != "NA"]))), "NA")
  
  cluster_colors <- setNames(alpha(hcl(
    seq(15, 375, length.out = n_distinct(cluster_names)), 80, 80
  ), 1), unique(cluster_names))
  
  html_head <- "<!DOCTYPE html>\n<html>\n<head>\n</head>\n\n<body>\n"
  
  html_foot <- "</body>\n</html>"
  
  alpha_name <- paste("alpha_", group, sep = "")
  
  generate_document_html <- function(data) {
    data$word[data$word == "\\n"] <- "<br>"
    data$word[data$word == "\\t"] <- "&nbsp;"
    
    document_html <- paste(
      paste(
        "<h3>ID: ",
        data$ID[1],
        "; ",
        replace_german_special_letters(employer_column),
        ": ",
        replace_german_special_letters(data[[employer_column]][1]),
        "<span style='color: red;'>",
        " (Signaling value: ",
        data$signaling_value[1],
        "; Ratio: ",
        data$ratio[1],
        ")</span></h3>",
        sep = ""
      ),
      paste(lapply(seq_len(nrow(data)), function(i) {
        if (is.na(data$cluster_name[i])) {
          paste(
            "<span>",
            replace_german_special_letters(data$word[i]),
            "</span>",
            ifelse(whitespace_after_needed(data$word[i], ifelse(i == nrow(data), NA, data$word[i + 1])), "&nbsp;", ""),
            sep = ""
          )
        } else {
          opacity <- ifelse(is.na(data[[alpha_name]][i]), 0.4, 0.4 + 0.6 * data[[alpha_name]][i])
          rgba_values <- paste(as.vector(col2rgb(cluster_colors[data$cluster_name[i]])), collapse = ",")
          word_style <- ifelse(is.na(data[[alpha_name]][i]), "", "font-weight: bold;")
          paste(
            "<span style='background-color:rgba(",
            rgba_values,
            ",",
            opacity,
            ");",
            word_style,
            "' title='Cluster: ",
            data$cluster_name[i],
            "; Cohens d: ",
            data$Cohens_d[i],
            "'>",
            replace_german_special_letters(data$word[i]),
            "</span>",
            ifelse(whitespace_after_needed(data$word[i], ifelse(i == nrow(data), NA, data$word[i + 1])), "&nbsp;", ""),
            sep = ""
          )
        }
      }),
      collapse = ""),
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
  
  final_html <- c(html_head, legend_html, document_html_list, html_foot)
  
  final_html_string <- paste(final_html, collapse = "\n")
  
  output_file <- paste("output/html_output_", group, ".html", sep = "")
  writeLines(final_html_string, output_file)
}

log_likelihood <-
  function(n_public, n_private, E_public, E_private) {
    result <-
      2 * ((n_public * ifelse(
        n_public == 0 | E_public == 0, 0, log(n_public / E_public)
      )) +
        (n_private * ifelse(
          n_private == 0 | E_private == 0, 0, log(n_private / E_private)
        )))
    return(result)
  }

log_ratio <-
  function(norm_public,
           public_total_words,
           private_total_words,
           norm_private) {
    zero_frequency <- 0.5
    result <-
      log2((
        ifelse(
          norm_public == 0,
          zero_frequency / public_total_words,
          norm_public
        )
      ) /
        (
          ifelse(
            norm_private == 0,
            zero_frequency / private_total_words,
            norm_private
          )
        ))
    return(result)
  }

# ------------------------ Load datasets and sampling ------------------------ #
# Public
public_ads_raw <- read.csv(PUBLIC_ADS_FILE, na.strings = c(""))
public_ads_sample_raw <- sample_first_n_ids(public_ads_raw)
rm(public_ads_raw)
public_ads_cleansed <-
  get_cleansed_vocab(public_ads_sample_raw, "Behörde")

# Adjust for cleansed IDs, number of IDs might be lower than given sample size
public_ads_sample_raw <-
  public_ads_sample_raw[public_ads_sample_raw$ID %in% unique(public_ads_cleansed$ID),]
real_sample_size_public <- length(unique(public_ads_cleansed$ID))

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

private_ads_sample_raw <-
  private_ads_sample_raw[private_ads_sample_raw$ID %in% unique(private_ads_cleansed$ID),]
real_sample_size_private <- length(unique(private_ads_cleansed$ID))

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
ggsave("output/zipfs_law.pdf", width = 12, height = 8, units = "cm", dpi = "print")

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
  thesaurus_distance_matrix(unique_words_public$lemma, group = "public", load = FALSE)
private_distance_matrix_thesaurus <-
  thesaurus_distance_matrix(unique_words_private$lemma,
                            group = "private",
                            load = FALSE)

# Addition of matrices, add weights if results getting better
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
dend_labels_public <- cluster_labels_dend(public_combined_distance_matrix)
public_labels_df <- cluster_labels_df(dend_labels_public, 13)

# Export csv for human-adjustments
write.csv(public_labels_df, "input/public_labels.csv", row.names = FALSE, quote = FALSE)
public_labels_df <- read.csv("input/public_labels.csv", header = TRUE, sep = ",")


dend_labels_private <- cluster_labels_dend(private_combined_distance_matrix)
private_labels_df <- cluster_labels_df(dend_labels_private, 11.5)

write.csv(private_labels_df, "input/private_labels.csv", row.names = FALSE, quote = FALSE)
private_labels_df <- read.csv("input/private_labels.csv", header = TRUE, sep = ",") 

# --------- Label replacement and Cluster Extraction using Word2Vec ---------- #
public_ads_cleansed_labels <- public_ads_cleansed %>%
  left_join(public_labels_df, by = c('lemma' = 'words'))

private_ads_cleansed_labels <- private_ads_cleansed %>%
  left_join(private_labels_df, by = c('lemma' = 'words'))

public_cluster_dend <- cluster_tagging_dend(public_ads_cleansed_labels, public_labels_df)
public_cluster <- cluster_tagging_df(public_cluster_dend, public_labels_df, "public", 2.88)

# Export csv for human-adjustments
write.csv(public_cluster, "input/public_cluster.csv", row.names = FALSE, quote = FALSE)
public_cluster <- read.csv("input/public_cluster.csv", header = TRUE, sep = ",") 

private_cluster_dend <- cluster_tagging_dend(private_ads_cleansed_labels, private_labels_df)
private_cluster <- cluster_tagging_df(private_cluster_dend, private_labels_df, "private", 2.73)

write.csv(private_cluster, "input/private_cluster.csv", row.names = FALSE, quote = FALSE)
private_cluster <- read.csv("input/private_cluster.csv", header = TRUE, sep = ",") 

# ---------------------------- Corpora comparison ---------------------------- #
# After manual inspection, assign names for clusters

public_cluster_names <-
  c(
    "Application",
    "Requirements",
    "Conditions",
    "Conditions",
    "Requirements",
    "Requirements",
    "Tasks",
    "Tasks",
    "Company"
  )

private_cluster_names <-
  c(
    "Requirements",
    "Requirements",
    "Tasks",
    "Tasks",
    "Company",
    "Company",
    "Conditions",
    "Application",
    "Company",
    "Tasks",
    "Company",
    "Requirements",
    "Company",
    "Company",
    "Tasks",
    "Application",
    "Requirements",
    "Tasks",
    "Company",
    "Tasks",
    "Tasks",
    "Tasks",
    "Company"
  )

public_cluster_names <-
  data.frame(cluster_nr = unique(public_cluster$cluster),
             cluster_name = public_cluster_names)

private_cluster_names <-
  data.frame(cluster_nr = unique(private_cluster$cluster),
             cluster_name = private_cluster_names)

public_cluster <- public_cluster %>%
  left_join(public_cluster_names, by = c('cluster' = 'cluster_nr'))

private_cluster <- private_cluster %>%
  left_join(private_cluster_names, by = c('cluster' = 'cluster_nr'))

public_ads_sample_raw_clusters <- public_ads_sample_raw %>%
  left_join(public_cluster, by = c('lemma' = 'words'))

private_ads_sample_raw_clusters <- private_ads_sample_raw %>%
  left_join(private_cluster, by = c('lemma' = 'words'))

# ------------------------------- Descriptives ------------------------------- #
public_cluster_desc <- public_ads_sample_raw_clusters %>%
  group_by(ID, cluster_name) %>%
  summarise(n_cluster_words = n()) %>%
  ungroup() %>%
  filter(n_cluster_words > 20) %>%
  group_by(cluster_name) %>%
  summarise(n_ids = n(),) %>%
  ungroup() %>%
  mutate(n_ids_rel = n_ids / real_sample_size_public)

private_cluster_desc <- private_ads_sample_raw_clusters %>%
  group_by(ID, cluster_name) %>%
  summarise(n_cluster_words = n()) %>%
  ungroup() %>%
  filter(n_cluster_words > 20) %>%
  group_by(cluster_name) %>%
  summarise(n_ids = n(),) %>%
  ungroup() %>%
  mutate(n_ids_rel = n_ids / real_sample_size_private)

descriptives <-
  left_join(
    public_cluster_desc,
    private_cluster_desc,
    by = "cluster_name",
    suffix = c("_public", "_private")
  )

# TODO: Independent t test

# TODO: Sequence of clusters?

# ----------------------------- Word differences ----------------------------- #
joined_words <-
  full_join(
    unique_words_public,
    unique_words_private,
    by = "lemma",
    suffix = c("_public", "_private")
  )
joined_words[is.na(joined_words)] <- 0

joined_words <- joined_words %>%
  mutate(over_underuse = ifelse(n_public > n_private, 1,-1)) %>%
  mutate(E_public = round(as.numeric(public_total_words) * (n_public + n_private) / (public_total_words + private_total_words), digits = 3)) %>%
  mutate(E_private = round(as.numeric(private_total_words) * (n_public + n_private) / (public_total_words + private_total_words), digits = 3)) %>%
  mutate(norm_public = round(n_public / public_total_words, digits = 7)) %>%
  mutate(norm_private = round(n_private / private_total_words, digits = 7)) %>%
  mutate(Log_likelihood = round(log_likelihood(n_public, n_private, E_public, E_private), digits = 3)) %>%
  mutate(Log_ratio = round(log_ratio(
    norm_public,
    public_total_words,
    private_total_words,
    norm_private
  ), digits = 3)) %>%
  mutate(Cohens_d = round(Log_ratio * sqrt(3) / pi, digits = 3))

# Log likelihood
# See: https://ucrel.lancs.ac.uk/llwizard.html
# 95th percentile; 5% level; p < 0.05; critical value = 3.84
# 99th percentile; 1% level; p < 0.01; critical value = 6.63
# 99.9th percentile; 0.1% level; p < 0.001; critical value = 10.83
# 99.99th percentile; 0.01% level; p < 0.0001; critical value = 15.13

joined_words_99_99 <-
  joined_words[joined_words$Log_likelihood > 15.13, ]

# Effect size filter
# See: Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed). Hillsdale, N.J: L. Erlbaum Associates.
# No effect; |d| < 0.2
# Small effect; 0.2 <= |d| < 0.5
# Medium effect; 0.5 <= |d| < 0.8
# Large effect; |d| >= 0.8

joined_words_99_99_large_effect <- joined_words_99_99[abs(joined_words_99_99$Cohens_d) >= 0.8, ]
write.csv(joined_words_99_99_large_effect, "output/signaling_words_sig_leffect", row.names=FALSE)

# Show via alpha parameter, 0 = transparent, 1 = opaque
joined_words_99_99_large_effect <- joined_words_99_99_large_effect %>%
  mutate(alpha_public = if_else(over_underuse == 1, round(normalizer(Cohens_d), digits = 2), NA)) %>%
  mutate(alpha_private = if_else(over_underuse == -1, round(normalizer(-Cohens_d), digits = 2), NA))

public_signaling_values <- public_ads_cleansed %>%
  left_join(joined_words_99_99_large_effect %>% select(lemma, Cohens_d, over_underuse)) %>%
  group_by(ID) %>%
  summarise(signaling_value = round(sum(Cohens_d, na.rm = TRUE) / first(words_in_id), digits = 3),
            ratio = paste(sum(over_underuse == 1, na.rm = TRUE), "/", sum(over_underuse == -1, na.rm = TRUE)))

# Norm to match Intention to apply scaling
public_signaling_values$signaling_value <- round(normalizer(public_signaling_values$signaling_value) * 5, digits = 3)

public_ads_sample_raw_clusters <- public_ads_sample_raw_clusters %>%
  left_join(joined_words_99_99_large_effect %>% select(lemma, alpha_public, Cohens_d)) %>%
  left_join(public_signaling_values)

private_signaling_values <- private_ads_cleansed %>%
  left_join(joined_words_99_99_large_effect %>% select(lemma, Cohens_d, over_underuse)) %>%
  group_by(ID) %>%
  summarise(signaling_value = round(sum(Cohens_d, na.rm = TRUE) / first(words_in_id), digits = 3),
            ratio = paste(sum(over_underuse == 1, na.rm = TRUE), "/", sum(over_underuse == -1, na.rm = TRUE)))

private_signaling_values$signaling_value <- round((normalizer(private_signaling_values$signaling_value) - 1) * 5, digits = 3)

private_ads_sample_raw_clusters <- private_ads_sample_raw_clusters %>%
  left_join(joined_words_99_99_large_effect %>% select(lemma, alpha_private, Cohens_d)) %>%
  left_join(private_signaling_values)

# ------------------------------- HMTL Output -------------------------------- #
public_ads_sample_raw_clusters_sorted <- public_ads_sample_raw_clusters %>%
  group_by(ID, signaling_value) %>%
  arrange(desc(signaling_value)) %>%
  ungroup()

create_html_output(public_ads_sample_raw_clusters_sorted, "public", "Behörde", 10)

private_ads_sample_raw_clusters_sorted <- private_ads_sample_raw_clusters %>%
  group_by(ID, signaling_value) %>%
  arrange(signaling_value) %>%
  ungroup()

create_html_output(private_ads_sample_raw_clusters_sorted, "private", "arbeitgeber", 10)