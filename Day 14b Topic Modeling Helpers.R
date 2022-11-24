#helper functions for text analysis

#enter the name of your GAMMA results dataframe
topic_clusters<-function(x){
  a<-as.matrix(ap_documents %>% tidyr::pivot_wider(id_cols=document,names_from = topic,values_from = gamma) %>%
                 select(-document))
  plot(hclust(dist(t(a),method="euclidian")))
}

#a function that takes the name of your dataset, THEN in quotes, the name of the column with texts
corpus_to_dtm<-function(x,y){
  song_dfm<-quanteda::dfm(paste(x,"$",y,sep=""), verbose=FALSE)
  D <-tidytext::tidy(song_dfm)
  more<-data.frame(word=c("(,",")",",","'","?","like","."))
D<-D %>% 
  rename("word"="term") %>% 
  anti_join(tidytext::get_stopwords()) %>% 
  anti_join(more) %>% 
  rename("term"="word")
corpus_dtm<<-D%>%
  tidytext::cast_dtm(document, term, count)}

corpus_to_dtm(songsC, "song_lyric")
