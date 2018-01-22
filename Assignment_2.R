rm(list = ls())

if (!require(igraph)) {install.packages("igraph")}
if (!require(tidytext)) {install.packages("tidytext")}
if (!require(data.table)) {install.packages("data.table")}
if (!require(stringr)) {install.packages("stringr")}
if (!require(tm)) {install.packages("tm")}
if (!require(RWeka)) {install.packages("RWeka")}
if (!require(wordcloud)) {install.packages("wordcloud")}
if (!require(ggplot2)) {install.packages("ggplot2")}
if (!require(dplyr)) {install.packages("dplyr")}
library(tidytext)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)
library(igraph)
library(dplyr)


################# Function 1 - TExt Cleaning


text_clean <- function(corpus,
                       sto){ 
  require(tidytext)
  library(tidytext)
  require(tm)
  library(dplyr)
  library(tm)
  sto = readLines(choose.files()) #choose text file for user defined stop words
  stop_wrds     <-   unique(c(sto , stop_words$word))
  x          <-   corpus
  x  =  gsub("<.*?>", " ", x)                  # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub=" ")   # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)       # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  gsub("^\\s+|\\s+$", " ", x)          # remove leading and trailing white space
  x = removeWords(x,stop_wrds)
  x  =  stripWhitespace(x)                  # removing white space
  textdf        <-   data_frame(document = 1:length(x) , text = x) 
  return(textdf)

} 


#Function 2a :Gives DTM matrix usign Term Frequency
dtm_tf <- function(x_df){
  corpus_words<-x_df %>% unnest_tokens(word,text) %>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words<-left_join(corpus_words,total_words)
  dtm_tf <-corpus_words %>% cast_dtm(document , word, n)
  
  return(dtm_tf)
}
#Function 2b :Gives DTM matrix usign Term Frequency
dtm_build_tf_idf() <- function(x_df){
  
  require(tidytext)
  library(tidytext)
  
  text_df<-x_df
  corpus_words<-text_df %>% unnest_tokens(word,text) %>% count(document,word,sort = TRUE)%>% ungroup()
  total_words<-corpus_words%>%group_by(document)%>%summarize(total=sum(n))
  corpus_words<-left_join(corpus_words,total_words)
  corpus_words<-corpus_words%>% bind_tf_idf(word,document,n)
  dtm_tf_idf <-corpus_words %>% cast_dtm(document , word, idf)
  
  return(dtm_tf_idf)
}


####
#Wite 3 functions for Word Clud , Bar Graph , Distilled COG



##Word Cloud Function
Plot_Wordc = function(dtm){    # plot wordcloud func opens. 
  library(wordcloud)
  dtm = as.matrix(dtm)  
  dtm_colsum = apply(dtm, 2, sum)  
  
  min1 = min(120, length(dtm_colsum))  
  words = colnames(dtm)[1:min1]  
  freq = 10 * dtm_colsum/mean(dtm_colsum)  # rescaling for better viewing
  wordcloud(words,  # wordcloud func begins
            freq,           
            scale = c(8, 0.3),  # can change this to adjust font scale
            colors=1:15)        # randomly choose between 15 colors
} # func ends

################  Bar Graphs Fuction #########

######### ggplot
Plot_BarChart = function(dtm){
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  wf <- data.frame(word=names(freq), freq=freq)
  # Plot Histogram
 bp <-  subset(wf, freq>1) %>%   
    ggplot(aes(word, freq)) +
    geom_bar(stat="identity", fill="red", colour="green") +
    theme(axis.text.x=element_text(angle=45, hjust=3))
 plot(bp, 
      layout = layout.kamada.kawai)
  } 


################ COG

Plot_distill_cog = function(dtm, # input DTM  
                            title, # title for the graph
                            s,    # no. of central nodes
                            k1){  # max no. of connections  
  s<-5
  k1<-10
  
  dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
  adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
  diag(adj.mat) = 0     # no self-references. So diag is 0.
  a0 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
  adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]]) 
  mat1 = adj.mat
  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # Plot_distill_cog  func ends

total_plot <- function(x){  
  dtm<-x
  dtm%>%Plot_Wordc() 
  dtm%>%Plot_BarChart()
  dtm%>%Plot_distill_cog(title,s,k1)
  
}

Corpus = readLines(choose.files()) #choose corpus
Cleanedtext_df <-text_clean(Corpus)


dtm = dtm_tf(Cleanedtext_df)
total_plot(dtm)

Plot_BarChart(dtm)
