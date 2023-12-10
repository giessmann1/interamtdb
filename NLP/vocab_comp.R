library(dplyr)
library(tidytext)
library(tidyr)

setwd("~/interamtdb/NLP")
public_vocab <- read.csv("public_vocab.csv", header = TRUE, na.strings = c(""))
private_vocab <- read.csv("private_vocab.csv", header = TRUE, na.strings = c(""))

# Public vocab
public_words <- public_vocab %>%
  drop_na() %>%
  count(ID, lemma, tag, sort = TRUE)

public_total_words <- public_words %>% group_by(ID) %>% summarize(total = sum(n))

public_words <- public_words %>%
  left_join(public_total_words) %>%
  bind_tf_idf(lemma, ID, n) %>%
  group_by(lemma, tag) %>%
  summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

# Private vocab
private_words <- private_vocab %>%
  drop_na() %>%
  count(ID, lemma, tag, sort = TRUE)

private_total_words <- private_words %>% group_by(ID) %>% summarize(total = sum(n))

private_words <- private_words %>%
  left_join(private_total_words) %>%
  bind_tf_idf(lemma, ID, n) %>%
  group_by(lemma, tag) %>%
  summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

# Join words
joined_words <- full_join(public_words, private_words,by = c('lemma', 'tag'), suffix = c("_public", "_private"))

joined_words <- joined_words %>%
  drop_na() %>%
  mutate(log_ratio = log(tf_idf_sum_public / tf_idf_sum_private))

library(ggplot2)
library(hrbrthemes)

ggplot(joined_words, aes(x=log_ratio)) +
  geom_histogram(binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9)
