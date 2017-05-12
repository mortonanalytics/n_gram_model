##tidy data management packages
library(dplyr)
library(tidyr)
library(tidytext)
library(quanteda)
##language detection package
library(textcat)
##graphics package
library(ggplot2)

dir <- "Z:/Ryan/Coursera/10_capstone_project/corpus/final/en_US"
setwd(dir)

data(stop_words)

docs <- list.files(dir)
docs <- docs[grep("txt", docs)]

fname <- paste(dir,"/",docs[1:3], sep = "")

text <- lapply(fname, readLines)

text_df <- data_frame(index = 1:length(text[[1]]),text = text[[1]]) %>% sample_frac(.3, replace = FALSE)
text_df <- rbind(text_df,data_frame(index = 1:length(text[[2]]),text = text[[2]]) %>% sample_frac(.25, replace = FALSE))
text_df <- rbind(text_df,data_frame(index = 1:length(text[[3]]),text = text[[3]]) %>% sample_frac(.15, replace = FALSE))

text_df$text <- gsub("_","", text_df$text)
# text_df$text <- gsub("-"," ", text_df$text)
text_df$text <- gsub("[^[:alpha:],.]", " ", text_df$text)
# text_df$text <- gsub("@","", text_df$text)
# text_df$text <- gsub("'","", text_df$text)


word_counts <- text_df %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  #filter(!word %in% stop_words) %>%
  ungroup()

words <- sum(word_counts$n)

word_counts <- word_counts %>%
  mutate(tf = n/words) %>%
  arrange(desc(tf)) %>%
  mutate(cum_tf = cumsum(tf))

words <- word_counts$word[word_counts$cum_tf < .8]
write.csv(word_counts, "unigram.csv",  row.names = FALSE)

#######N-Gram analysis 
text_2_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 2)  %>%
  separate(n_gram, c("word1", "word2"), sep = " ") %>%
  # filter(word1 %in% words) %>%
  # filter(word2 %in% words) %>%
  count(word1, word2) %>%
  bind_tf_idf(word2, word1, n) %>%
  ungroup()
write.csv(text_2_grams, "bigram.csv", row.names = FALSE)

text_3_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 3)  %>%
  separate(n_gram, c("word1", "word2", "word3"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  unite(key,word1,word2, sep = " ") %>%
  count(key, word3) %>%
  bind_tf_idf(word3, key, n)%>%
  ungroup()
write.csv(text_3_grams, "trigram.csv", row.names = FALSE)

text_4_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 4)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  unite(key,word1,word2,word3, sep = " ") %>%
  count(key, word4) %>%
  bind_tf_idf(word4, key, n)%>%
  ungroup()
write.csv(text_4_grams, "4_gram.csv", row.names = FALSE)

text_5_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 5)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word5"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  # filter(!word5 %in% words) %>%
  unite(key,word1,word2,word3, word4, sep = " ") %>%
  count(key, word5) %>%
  bind_tf_idf(word5, key, n)%>%
  ungroup()
write.csv(text_5_grams, "5_gram.csv", row.names = FALSE)

text_6_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 6)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  # filter(!word5 %in% words) %>%
  # filter(!word6 %in% words) %>%
  unite(key,word1,word2,word3, word4, word5, sep = " ") %>%
  count(key, word6) %>%
  bind_tf_idf(word6, key, n)%>%
  ungroup()
write.csv(text_6_grams, "6_gram.csv", row.names = FALSE)
