############ Functions for Sentiment Analysis
try(require(dplyr) || install.packages("dplyr",dependencies = TRUE))
try(require(tidyr) || install.packages("tidyr",dependencies = TRUE))
try(require(tidytext) || install.packages("tidytext",dependencies = TRUE))
try(require(tm) || install.packages("tm", dependencies = TRUE))
try(require(wordcloud) || install.packages("wordcloud",dependencies = TRUE))
try(require(ggplot2) || install.packages("ggplot2",dependencies = TRUE))
try(require(igraph) || install.packages("igraph",dependencies = TRUE))
try(require(widyr) || install.packages("widyr",dependencies = TRUE))
try(require(ggraph) || install.packages("ggraph",dependencies = TRUE))

library("tidytext")
library("dplyr")
library("tidyr")
library("tm")
library("wordcloud")
library("ggplot2")
library("igraph")
library("widyr")
library("ggraph")

################# Text Cleaning


#This fuction takes uncleaned corpus and returns cleaned corpus as Data Frame
text_clean <- function(corpus,
                       user_stpw ####User Defines Stop words
){ 
  require(tidytext)
  library(tidytext)
  require(tm)
  library(dplyr)
  library(tm)
  stop_wrds     <-   unique(c(user_stpw , stop_words$word))
  text          <-   corpus
  text          <-   gsub("<.*?>", " ", text)              # regex for removing HTML tags
  text          <-   iconv(text, "latin1", "ASCII", sub="") # Keep only ASCII characters
  text          <-   gsub("[^[:alnum:]]", " ",  text )        # keep only alpha numeric 
  text          <-   tolower(text) 
  text          <-   gsub("^\\s+|\\s+$", "", text)          # remove leading and trailing white space
  text          <-   removeWords(text,stop_wrds)
  text          <-   stripWhitespace(text)
  text          <-   gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)  #removing all extra spaces
  textdf        <-   data_frame(document = 1:length(text) , text = text) 
  return(textdf)
} 

# Sentiment Analysis - This functions performs sentiment analysis using AFINN , BING and NRC and plots the three outputs


sentiment_analysis<- function(data_frame ){
  textdf<- data_frame
  afinn <-textdf %>% ungroup() %>%
    unnest_tokens(word, text) %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = document%/% 1) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = "AFINN")
  
  bing_and_nrc <- bind_rows(textdf %>% ungroup() %>%
                              unnest_tokens(word, text) %>%
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing"),
                            textdf %>% ungroup() %>%
                              unnest_tokens(word, text)%>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
    count(method, index = document %/% 1, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  
  
  bind_rows(afinn, 
            bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method
    )) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
  
}



# This fuction returns various documents with in the corpora and count of specific sentiment in each of them.

specific_sent_count<- function(data_frame , sentiment_in_NRC ){
  nrc_semtiment <- get_sentiments("nrc") %>% 
    filter(sentiment == sentiment_in_NRC)
  

  k= ungroup(data_frame) %>%
    unnest_tokens(word, text) %>%
    inner_join(nrc_semtiment) %>%group_by(document)%>%
    count(document, sort = TRUE)
  
  return(k)
}



