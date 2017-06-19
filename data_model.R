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

text_df <- data_frame(index = 1:length(text[[1]]),text = text[[1]]) %>% sample_frac(.1, replace = FALSE)
text_df <- rbind(text_df,data_frame(index = 1:length(text[[2]]),text = text[[2]]) %>% sample_frac(.1, replace = FALSE))
text_df <- rbind(text_df,data_frame(index = 1:length(text[[3]]),text = text[[3]]) %>% sample_frac(.05, replace = FALSE))

test_df <- data_frame(index = 1:length(text[[1]]),text = text[[1]]) %>% sample_frac(.01, replace = FALSE)

text_df$text <- gsub("http[[:alnum:]]*", "", text_df$text)
text_df$text <- gsub("@[a-z,A-Z]*","", text_df$text)
text_df$text <- gsub("_","", text_df$text)
text_df$text <- iconv(text_df$text, to='ASCII//TRANSLIT')
text_df$text <- gsub("'", "", text_df$text)
# text_df$text <- gsub("-"," ", text_df$text)
text_df$text <- gsub("[^[:alpha:],.]", " ", text_df$text)
text_df$text <-gsub('([[:alpha:]])\\1+', '\\1', text_df$text)
text_df$text <- gsub("\\.", " <s> <s>", text_df$text)
text_df$text <- paste("<s>", text_df$text, sep = " ")


test_df$text <- gsub("http[[:alnum:]]*", "", test_df$text)
test_df$text <- gsub("@[a-z,A-Z]*","", test_df$text)
test_df$text <- gsub("_","", test_df$text)
test_df$text <- iconv(test_df$text, to='ASCII//TRANSLIT')
test_df$text <- gsub("'", "", test_df$text)
# test_df$text <- gsub("-"," ", test_df$text)
test_df$text <- gsub("[^[:alpha:],.]", " ", test_df$text)
test_df$text <-gsub('([[:alpha:]])\\1+', '\\1', test_df$text)
test_df$text <- gsub("\\.", " <s> <s>", test_df$text)
test_df$text <- paste("<s>", test_df$text, sep = " ")

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

words <- word_counts$word #[word_counts$cum_tf < .8]
write.table(words, "words.txt", row.names = FALSE)
#write.csv(word_counts, "unigram.csv",  row.names = FALSE)

#######N-Gram analysis 
text_2_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 2)  %>%
  separate(n_gram, c("key", "word"), sep = " ") %>%
  # filter(word1 %in% words) %>%
  # filter(word2 %in% words) %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n) %>%
  filter(n > 1) %>%
  ungroup()

text_3_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 3)  %>%
  separate(n_gram, c("word1", "word2", "word"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  unite(key,word1,word2, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

text_4_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 4)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  unite(key,word1,word2,word3, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

text_5_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 5)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  # filter(!word5 %in% words) %>%
  unite(key,word1,word2,word3, word4, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

text_6_grams <- text_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 6)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word5", "word"), sep = " ") %>%
  # filter(!word1 %in% words) %>%
  # filter(!word2 %in% words) %>%
  # filter(!word3 %in% words) %>%
  # filter(!word4 %in% words) %>%
  # filter(!word5 %in% words) %>%
  # filter(!word6 %in% words) %>%
  unite(key,word1,word2,word3, word4, word5, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

n_gram_df <- do.call("rbind", list(text_2_grams, text_3_grams, text_4_grams, text_5_grams, text_6_grams))
write.csv(n_gram_df, "n_gram.csv", row.names = FALSE)

###remove stop words set
library(tm)

text_df_2_corpus <- Corpus(DataframeSource(text_df))
text_df_2_corpus <- tm_map(text_df_2_corpus,removeWords, stop_words$word)

text_df_2 <- tidy(text_df_2_corpus) %>% select(text) %>% separate(text, c("index", "text"), sep = "\\n")

word_counts_stop <- text_df_2 %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  #filter(!word %in% stop_words) %>%
  ungroup()

words_stop <- sum(word_counts_stop$n)


word_counts_stop <- word_counts_stop %>%
  mutate(tf = n/words_stop) %>%
  arrange(desc(tf)) %>%
  mutate(cum_tf = cumsum(tf))

words_stop <- word_counts_stop$word #[word_counts$cum_tf < .8]
write.table(words_stop, "words_stop.txt", row.names = FALSE)

stop_2_grams <- text_df_2 %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 2)  %>%
  separate(n_gram, c("key", "word"), sep = " ") %>%
  # filter(key %in% words_stop) %>%
  # filter(word %in% words_stop) %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n) %>%
  filter(n > 1) %>%
  ungroup()

stop_3_grams <- text_df_2 %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 3)  %>%
  separate(n_gram, c("word1", "word2", "word"), sep = " ") %>%
  # filter(!word1 %in% words_stop) %>%
  # filter(!word2 %in% words_stop) %>%
  # filter(!word %in% words_stop) %>%
  unite(key,word1,word2, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()


stop_4_grams <- text_df_2 %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 4)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word"), sep = " ") %>%
  # filter(!word1 %in% words_stop) %>%
  # filter(!word2 %in% words_stop) %>%
  # filter(!word3 %in% words_stop) %>%
  # filter(!word %in% words_stop) %>%
  unite(key,word1,word2,word3, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

stop_5_grams <- text_df_2 %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 5)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word"), sep = " ") %>%
  # filter(!word1 %in% words_stop) %>%
  # filter(!word2 %in% words_stop) %>%
  # filter(!word3 %in% words_stop) %>%
  # filter(!word4 %in% words_stop) %>%
  # filter(!word %in% words_stop) %>%
  unite(key,word1,word2,word3, word4, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

stop_6_grams <- text_df_2 %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 6)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word5", "word"), sep = " ") %>%
  # filter(!word1 %in% words_stop) %>%
  # filter(!word2 %in% words_stop) %>%
  # filter(!word3 %in% words_stop) %>%
  # filter(!word4 %in% words_stop) %>%
  # filter(!word5 %in% words_stop) %>%
  # filter(!word %in% words_stop) %>%
  unite(key,word1,word2,word3, word4, word5, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

n_gram_df_stop <- do.call("rbind", list(stop_2_grams, stop_3_grams, stop_4_grams, stop_5_grams, stop_6_grams))
write.csv(n_gram_df_stop, "n_gram_stop.csv", row.names = FALSE)

##########test set
test_6_grams <- test_df %>%
  unnest_tokens(n_gram, text, token = "ngrams", n = 6)  %>%
  separate(n_gram, c("word1", "word2", "word3", "word4", "word5", "word"), sep = " ") %>%
  # filter(!word1 %in% words_stop) %>%
  # filter(!word2 %in% words_stop) %>%
  # filter(!word3 %in% words_stop) %>%
  # filter(!word4 %in% words_stop) %>%
  # filter(!word5 %in% words_stop) %>%
  # filter(!word %in% words_stop) %>%
  unite(key,word1,word2,word3, word4, word5, sep = " ") %>%
  count(key, word) %>%
  bind_tf_idf(word, key, n)%>%
  filter(n > 1) %>%
  ungroup()

test_6_grams <- test_6_grams %>%
  mutate(pred = select.word(key)) %>%
  mutate(result = ifelse(word == pred, 1, 0))

write.csv(test_6_grams, "test_6_grams.csv", row.names = FALSE)
