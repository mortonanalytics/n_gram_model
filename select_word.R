##tidy data management packages
library(dplyr)
library(tidyr)

dir <- "Z:/Ryan/Coursera/10_capstone_project/corpus/final/en_US"
setwd(dir)

key <- read.csv("dictionary.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

unknown.word <- function(word) {
  word <- ifelse(word %in% words, word, "<unknown>")
}

select.word <- function(x1, x2=NULL, x3 = NULL){
  if(is.null(x2)){
    word <- key %>% filter(word1 == x1) %>% select(word2) 
  } else if(is.null(x3)){
    word <- key %>% filter(word1 == x1 & word2 == x2) %>% select(word3)
    } else {
      word <- key %>% filter(word1 == x1 & word2 == x2 & word3 == x3) %>% select(word4)
  }
  word[1:2,1]
}

