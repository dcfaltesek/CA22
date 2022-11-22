library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(stringr)

#to start we will use the classic AP stories example
#this is an antique
data("AssociatedPress")

#this object is a DTM
AssociatedPress


#this is our core mechanic - we pass our DTM to LDA; K is number of topics.
#this is an antique example designed for a two topic spread
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

#cross-check and verify
ap_lda

#did we pick the right number of topics
perplexity(ap_lda)


#and what are those
terms(ap_lda, k=5)


#but which words...
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#this cute but largely useless for in situ models 
beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#OK here is the problem with tidytext, they don't rep that this is what we all wanted
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#and this is the real deep key - NOT IN THE TEXTBOOK
ap_wide<-ap_documents %>%
  pivot_wider(id_cols=document, 
              names_from = topic,
              values_from = c(gamma))

ap_wide





#now here is the next key step...
#we need to convert 


#import with a different name...

#first we will just combined the lyric lines back into a single object
songsB<-songs %>% 
  group_by(track_name) %>% 
  select(c(album_name,track_name,song_lyric))

#this is quanteda code that we are using as a loader
#NEW SKILL - CALLING FUNCTIONS FRMO AN UNATTACHED PACKAGE
song_corpus<-quanteda::corpus(songsB$song_lyric, docvars = data.frame(song=songsB$track_name, album=songsB$album_name))

#now we can go ahead and call the object, read the results carefully
song_corpus

#we can now use this with QUANTEDA methods (more on those on Wednesday)

#Let's use the songs then and model those
#quanteda can allow export to many distinct methods

#three distinct steps
#corpus to DFM
song_dfm<-quanteda::dfm(song_corpus, verbose=FALSE)
#tidy it
D <-tidy(song_dfm)

get_stopwords()

more<-data.frame(word=c("(,",")",",","'","?","like"))
D<-D %>% 
  rename("word"="term") %>% 
  anti_join(get_stopwords()) %>% 
  anti_join(more) %>% 
  rename("term"="word")
  
  
#output as a DTM
song_dtm<-D%>%
  cast_dtm(document, term, count)


#now what...


song_lda <- LDA(song_dtm, k = 2, control = list(seed = 1234))
tm_documents <- tidy(song_lda, matrix = "gamma")



#from here on it gets into special dan territory 
tm_wide<-tm_documents %>%
  pivot_wider(id_cols=document, 
              names_from = topic,
              values_from = c(gamma))

#and now that we have done the hard part, here is where we go big
#now we need to back-connect our data, so we are going to do some stuff here with STRINGR
#if you get gud with regex, good job
tm_songs<-tm_wide %>% 
  #there is a ton of stuff happening in this line
  #it is a mutate, so it makes a new line
  #the result is named what the reference column was in the original dataset
  #this line is a pre-processor
  mutate(six_id=as.double(str_replace_all(document, "text", ""))) %>% 
  #this is the business end line of the cluster
  inner_join(songs)
View(tm_songs)

#this next vector should be equal to the number of topics 
A<-1:2
#that is not quite graphable, so now this
tm_processed<-tm_songs %>% 
  #let's just drop the text stuff
  select(-c(document, album_name, song_lyric, track_name)) %>% 
  #pivot down the probabilites and topic assignments
  #there is great evil in this next line, look at that PASTE command 
  #it is pasting the vector A into this function, which then includes the tokens to call all the topic lines
  pivot_longer(cols=c(paste(A,sep = ",")),
               names_to = "topic",
               values_to = "prob") %>%
  #PIVOT IT AGAIN to put the artist names in a colum too
  #I wrote out the string here, but I could have pasted the column names selected using absolute value from base R
         pivot_longer(cols=c(Taylor.Swift, Chili.Peppers, Foo.Fighters, Lil.Wayne, Vulfpeck, Kanye.West),names_repair = "minimal") %>% 
  #and lets only look at stuff above .7
  filter(prob > .99)

tm_processed %>% 
  ggplot(aes(as.double(topic), prob, colour=name, alpha=value))+geom_jitter()+facet_wrap(~name)

#songs beta
tm_topics <- tidy(song_lda, matrix = "beta")
tm_top_terms <- tm_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#cluttered by useful
tm_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#we can see that topic seven has some distinct features...
tm_processed %>% 
  filter(topic==7) %>% 
  filter(value==1) %>% 
  ggplot(aes(six_id, prob, colour=name))+geom_jitter()
  
#let's try a count method
tm_processed %>% 
  filter(value ==1) %>% 
  group_by(name) %>% 
  count(topic) %>% 
  arrange(desc(n))

#top of each category...
tm_processed %>% 
  filter(value==1) %>% 
  group_by(topic) %>% 
  top_n(5, prob)


#and what words are those...
terms(song_lda, k=5)
