library(dplyr)
library(tidytext)
library(tidyr)

setwd("~/interamtdb/NLP")
public_vocab <- read.csv("public_vocab.csv", header = TRUE, na.strings = c(""))
private_vocab <- read.csv("private_vocab.csv", header = TRUE, na.strings = c(""))

# Public vocab
public_words <- public_vocab %>%
  drop_na() %>%
  mutate(tag = gsub("\\(.+\\)", "", tag)) %>%
  count(ID, lemma, tag, sort = TRUE)

public_total_words <- public_words %>% group_by(ID) %>% summarize(total = sum(n))

public_words <- public_words %>%
  left_join(public_total_words) %>%
  bind_tf_idf(lemma, ID, n) %>%
  group_by(lemma, tag) %>%
  summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

public_words <- public_words[public_words$n >= 4, ]

# Private vocab
private_words <- private_vocab %>%
  drop_na() %>%
  mutate(tag = gsub("\\(.+\\)", "", tag)) %>%
  count(ID, lemma, tag, sort = TRUE)

private_total_words <- private_words %>% group_by(ID) %>% summarize(total = sum(n))

private_words <- private_words %>%
  left_join(private_total_words) %>%
  bind_tf_idf(lemma, ID, n) %>%
  group_by(lemma, tag) %>%
  summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

private_words <- private_words[private_words$n >= 4, ]

# Join words
joined_words <- full_join(public_words, private_words,by = c('lemma', 'tag'), suffix = c("_public", "_private"))

# Exclusives
PPDs_exlusive <- joined_words[is.na(joined_words$n_private), ]
PPDs_exlusive_adj <- PPDs_exlusive[grepl("ADJ", PPDs_exlusive$tag), ]
PPDs_exlusive_adj <- PPDs_exlusive_adj[order(PPDs_exlusive_adj$tf_idf_sum_public, decreasing = TRUE), ]

PPDs_private_exlusive <- joined_words[is.na(joined_words$n_public), ]
PPDs_private_exlusive_adj <- PPDs_private_exlusive[grepl("ADJ", PPDs_private_exlusive$tag), ]
PPDs_private_exlusive_adj <- PPDs_private_exlusive_adj[order(PPDs_private_exlusive_adj$tf_idf_sum_private, decreasing = TRUE), ]

library(wordcloud)
library(RColorBrewer)

set.seed(1234)
wordcloud(words = PPDs_exlusive_adj$lemma, freq = abs(PPDs_exlusive_adj$tf_idf_sum_public)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

set.seed(1234)
wordcloud(words = PPDs_private_exlusive_adj$lemma, freq = abs(PPDs_private_exlusive_adj$tf_idf_sum_private)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

# Log ratio
joined_words <- joined_words %>%
  drop_na() %>%
  mutate(log_ratio = log(tf_idf_sum_public / tf_idf_sum_private))

lower_quantil <- quantile(joined_words$log_ratio, 0.10)
upper_quantil <- quantile(joined_words$log_ratio, 0.90)
mean <- mean(joined_words$log_ratio)

PPDs <- joined_words[joined_words$log_ratio > upper_quantil, ]
PPDs_adj <- PPDs[grepl("ADJ", PPDs$tag), ]
PPDs_adj <- PPDs_adj[order(PPDs_adj$log_ratio, decreasing = TRUE), ]

PPDs_private <- joined_words[joined_words$log_ratio < lower_quantil, ]
PPDs_private_adj <- PPDs_private[grepl("ADJ", PPDs_private$tag), ]
PPDs_private_adj <- PPDs_private_adj[order(PPDs_private_adj$log_ratio, decreasing = TRUE), ]
       
library(ggplot2)

ggplot(joined_words, aes(x=log_ratio)) +
  geom_histogram(binwidth=0.2, fill="#69b3a2", color="white", alpha=0.9) +
  geom_vline(xintercept = lower_quantil, linetype = "dashed") +
  geom_vline(xintercept = upper_quantil, linetype = "dashed") +
  geom_vline(xintercept = mean, color="red") +
  geom_text(aes(x=mean, label=round(mean, digits = 2), y=100), color="red") +
  theme_light() +
  ggtitle("Distribution of keyword log ratios (logarithmic ratio of the relative frequencies)") +
  xlab("Log Ratio (Negative = Private, Positive = Public)") +
  ylab("Frequency")

set.seed(1234)
wordcloud(words = PPDs_adj$lemma, freq = abs(PPDs_adj$log_ratio)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

set.seed(1234)
wordcloud(words = PPDs_private_adj$lemma, freq = abs(PPDs_private_adj$log_ratio)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.2,0.5))

 