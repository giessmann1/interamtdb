library(dplyr)
library(tidytext)
library(tidyr)

setwd("~/interamtdb/NLP")
public_vocab <- read.csv("public_vocab.csv", header = TRUE, na.strings = c(""))

length(unique(public_vocab$ID))

private_vocab <- read.csv("private_vocab.csv", header = TRUE, na.strings = c(""))

length(unique(private_vocab$ID))

# Public vocab
public_words <- public_vocab %>%
  drop_na() %>%
  mutate(tag = gsub("\\(.+\\)", "", tag)) %>%
  count(lemma, tag, Behörde, name = "lemma_by_behörde") %>%
  group_by(lemma, tag) %>%
  summarize(
    word_by_employers = n(),
    n = sum(lemma_by_behörde)
  )
public_words$word_by_employers <- as.numeric(public_words$word_by_employers)
public_words$n <- as.numeric(public_words$n)

# public_total_words <- public_words %>% group_by(ID) %>% summarize(total = sum(n))
public_total_words <- as.numeric(sum(public_words$n))

# public_words <- public_words %>%
#   left_join(public_total_words) %>%
#   bind_tf_idf(lemma, ID, n) %>%
#   group_by(lemma, tag) %>%
#   summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

# public_words <- public_words[public_words$n >= 4, ]

# Private vocab
private_words <- private_vocab %>%
  drop_na() %>%
  mutate(tag = gsub("\\(.+\\)", "", tag)) %>%
  count(lemma, tag, arbeitgeber, name = "lemma_by_arbeitgeber") %>%
  group_by(lemma, tag) %>%
  summarize(
    word_by_employers = n(),
    n = sum(lemma_by_arbeitgeber)
  )
private_words$word_by_employers <- as.numeric(private_words$word_by_employers)
private_words$n <- as.numeric(private_words$n)

# private_total_words <- private_words %>% group_by(ID) %>% summarize(total = sum(n))
private_total_words <- as.numeric(sum(private_words$n))

# private_words <- private_words %>%
#   left_join(private_total_words) %>%
#   bind_tf_idf(lemma, ID, n) %>%
#   group_by(lemma, tag) %>%
#   summarise(tf_idf_sum = sum(tf_idf), n = sum(n))

# private_words <- private_words[private_words$n >= 4, ]

# Join words
joined_words <- full_join(public_words, private_words,by = c('lemma', 'tag'), suffix = c("_public", "_private"))
joined_words[is.na(joined_words)] <- 0
joined_words$combined_word_by_employer = joined_words$word_by_employers_public + joined_words$word_by_employers_private
joined_words <- subset(joined_words, combined_word_by_employer > 2) 

library(ggplot2)

ggplot(joined_words, aes(x = combined_word_by_employer)) +
  geom_histogram(binwidth=2, fill="#69b3a2", color="white", alpha=0.9) +
  scale_y_log10() +
  theme_light() +
  xlab("Employers using a word nth times") +
  ylab("Frequency (log scale)")

## Calc helping variables
joined_words$O_public <- joined_words$n_public/public_total_words
joined_words$O_private <- joined_words$n_private/private_total_words

# Exclusives
PPDs_exlusive <- joined_words[is.na(joined_words$n_private), ]
PPDs_exlusive_adj <- PPDs_exlusive[grepl("ADJ", PPDs_exlusive$tag), ]
PPDs_exlusive_adj <- PPDs_exlusive_adj[order(PPDs_exlusive_adj$O_public, decreasing = TRUE), ]

PPDs_private_exlusive <- joined_words[is.na(joined_words$n_public), ]
PPDs_private_exlusive_adj <- PPDs_private_exlusive[grepl("ADJ", PPDs_private_exlusive$tag), ]
PPDs_private_exlusive_adj <- PPDs_private_exlusive_adj[order(PPDs_private_exlusive_adj$O_private, decreasing = TRUE), ]

library(wordcloud)
library(RColorBrewer)

set.seed(1234)
wordcloud(words = PPDs_exlusive_adj$lemma, freq = abs(PPDs_exlusive_adj$O_public)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(2, 0.5))

set.seed(1234)
wordcloud(words = PPDs_private_exlusive_adj$lemma, freq = abs(PPDs_private_exlusive_adj$O_private)*1000, max.words=50,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per=0, scale=c(1.4, 0.5))

# Log ratio

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

 