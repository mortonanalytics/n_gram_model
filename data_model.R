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
text_df <- data_frame(index = 1:length(text),text = text) 
# text_df$text <- gsub(pattern = '[^a-zA-Z0-9\\s]+',
#                      x = text_df$text,
#                      replacement = "",
#                      ignore.case = TRUE,
#                      perl = TRUE)

total_words <- text_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  summarise(total = sum(n))

word_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(freq = n/total_words$total,
         cum_sum = cumsum(n),
         cum_percent = cumsum(n)/total_words$total)

words <- word_counts$word[word_counts$cum_percent < .5]

text_trigrams <- text_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)  %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(word1 %in% words) %>%
  filter(word2 %in% words) %>%
  filter(word3 %in% words)

dictionary <- text_trigrams %>%
  count(word1, word2, word3, sort = TRUE) %>%
  mutate(freq = n/nrow(text_trigrams),
         cum_sum = cumsum(n/nrow(text_trigrams)))%>%
  #filter(cum_sum < .5) %>%
  arrange(desc(freq))
