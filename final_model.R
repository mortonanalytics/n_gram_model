library(data.table)

words <- read.table("./data/words.txt", stringsAsFactors = FALSE)
n_gram_df <- read.csv("./data/n_gram.csv", stringsAsFactors = FALSE)
n_gram_dt <- data.table(n_gram_df, key = "key")

# n_gram_df_stop <- read.csv("./data/n_gram_stop.csv", stringsAsFactors = FALSE)
# n_gram_dt_stop <- data.table(n_gram_df, key = "key")

unknown.word <- function(word) {
  word <- ifelse(word %in% words$V1, word, "<unk>")
}

select.word <- function(m){
  v <- unlist(strsplit(m, " "))
  v <- gsub("[^[:alpha:]]", " ", v)
  #v <- v[!v %in% stop_words$word]
  unigram <- unknown.word(v[length(v)])
  bigram <- paste(unknown.word(v[length(v)-1]), unknown.word(v[length(v)]), sep = " ")
  trigram <- paste(unknown.word(v[length(v)-2]),unknown.word(v[length(v)-1]), unknown.word(v[length(v)]), sep = " ")
  quadgram <- ifelse(length(v) >=3, 
                     paste(unknown.word(v[length(v)-3]),unknown.word(v[length(v)-2]),unknown.word(v[length(v)-1]), unknown.word(v[length(v)]), sep = " "),
                     " ")
  fivegram <- ifelse(length(v) >=4,
                     paste(unknown.word(v[length(v)-4]),unknown.word(v[length(v)-3]),unknown.word(v[length(v)-2]),unknown.word(v[length(v)-1]), unknown.word(v[length(v)]), sep = " "),
                     " ")
  
  key1 <- n_gram_dt[key == unigram]
  key2 <- n_gram_dt[key == bigram]
  key3 <- n_gram_dt[key == trigram]
  key4 <- n_gram_dt[key == quadgram]
  key5 <- n_gram_dt[key == fivegram]
  
  nextWord <- ifelse(nrow(key5) > 0, key5$word[key5$tf == max(key5$tf)],
                     ifelse(nrow(key4) > 0, key4$word[key4$tf == max(key4$tf)],
                          ifelse(nrow(key3) > 0 , key3$word[key3$tf == max(key3$tf)],
                                 ifelse(nrow(key2) > 0, key2$word[key2$tf == max(key2$tf)], 
                                        ifelse(nrow(key1) > 0, key1$word[key1$tf == max(key1$tf)],
                                               " ")))))
  nextWord <- ifelse(nextWord == "<s>", "\\.", nextWord)
  
  return(nextWord)
}
