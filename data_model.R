library(tm)
##tidy data management packages
library(dplyr)
library(tidyr)
library(tidytext)
##language detection package
library(textcat)
##graphics package
library(ggplot2)

dir <- "Z:/Ryan/Coursera/10_capstone_project/corpus/final/en_US"
setwd(dir)

docs <- list.files(dir)

fname <- paste(dir,"/",docs[1], sep = "")

text <- readLines(fname)
data("stop_words")
text_df <- data_frame(index = 1:length(text),text = text) 
text_df$text <- gsub(pattern = '[^a-zA-Z0-9\\s]+',
                     x = text_df$text,
                     replacement = "",
                     ignore.case = TRUE,
                     perl = TRUE)

total_words <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  summarise(total = sum(n))

word_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  mutate(freq = n/total_words$total,
         cum_sum = cumsum(n),
         cum_percent = cumsum(n)/total_words$total)

p <- ggplot(word_counts, aes(n/total_words$total)) +
  geom_histogram()

fifty_percent <- word_counts %>% filter(cum_percent < .5) %>% summarise(count = n())

ninety_percent <- word_counts %>% filter(cum_percent < .9) %>% summarise(count = n())
  
text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")

text_trigrams <- text_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) 

dictionary <- text_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  mutate(freq = n/nrow(text_trigrams),
         cum_sum = cumsum(n/nrow(text_trigrams)))%>%
  filter(cum_sum < .5) %>%
  arrange(desc(freq))
