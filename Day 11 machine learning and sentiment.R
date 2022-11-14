library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(textdata)
library(ggrepel)

#INSTALL ME
library(textdata)

#subset the text 
new_taylor<-taylor_5%>%
  filter(artist_name== "Taylor Swift")%>%
  filter(album_name == "folklore" | album_name=="1989")

#the length of your chosen data as a vector
taylor_lines<-data.frame(new_taylor, "reference"=1:dim(new_taylor)[1])

#word count per line
taylor_lines_counted <- taylor_lines %>%
  unnest_tokens(word, lyric) %>%
  count(reference, word, sort = TRUE)%>%
  rename(per_line = n)

#get scores
afinn<-get_sentiments("afinn")

#attach the scores
with_scores<-taylor_lines_counted%>%
  inner_join(afinn, by="word")


#create a line score
scores_per_line<-with_scores%>%
  group_by(reference)%>%
  #notice our per line strategy is SUM
  summarize(line_value=sum(value), line_var=sd(value))


#attach your sentiments
taylor_sentiments<-inner_join(taylor_lines, scores_per_line, by="reference")

#visualize the albums
albums<-taylor_sentiments%>%
  group_by(track_name)%>%
  #adds up the scores for each line, adds the track number, and then album name
  summarize(song_value=sum(line_value), song_sd=sd(line_value), album=album_name)%>%
  ungroup()

#this is an example of varience MATH STUFF
low<-c(1,-1,1,-1)
high<-c(5,-2,6,11)
sd(low)
sd(high)

#we can plot this
var<-data.frame(X=1:4, low, high)
#with linear method argument
ggplot(var, aes(X,high))+geom_smooth(method="lm")
#this is just some overfitting nonsense
ggplot(var, aes(X,high))+geom_smooth()


#removes dupes created by the album function
albums<-distinct(albums)
#take a look
View(albums)

#X is track number, Y is song value, color is album, this repel code makes it so the labels don't overlap when zoomed out
ggplot(albums, aes(track_name, song_value, colour=album))+geom_label_repel(aes(label=track_name))

#now inclues song varience calcualtion
ggplot(albums, aes(track_name, song_value, colour=album))+geom_label_repel(aes(size=song_sd, label=track_name))

#explore the causes
taylor_sentiments%>%
  filter(track_name=="Bad Blood")

#take a look inside the song
taylor_sentiments%>%
  filter(track_name=="Bad Blood")%>%
  ggplot(aes(reference, line_value, colour=line_var))+geom_jitter()



#TOPICS
#simplified topic model
library(topicmodels)

#standard tidy text analysis library
library(tidytext)

#helpful manipulation tools
library(dplyr)

#charts and graphs and stuff
library(ggplot2)

#various tidy commands
library(tidyr)

#this works with the today data

#STEP ONE: standard addition of document reference number
#this is just like what we did before, we can just go ahead and use taylor

#STEP TWO - count stuff #LETS JUST USE TAYLOR
today3<-today2 %>%
  unnest_tokens(word, Text) %>%
  count(reference, word, sort = TRUE)%>%
  rename(per_line = n)

#STEP THREE - convert data structure from TIDY to DOCUMENT TERM MATRIX
taylor_dtm<-taylor_lines_counted%>%
  cast_dtm(reference, word, per_line)

#STEP FOUR - run LDA
#this version of LDA does not allow control of alpha or beta assumptions
#these control the rate at which the model changes during the learning cycles
taylor_lda <- LDA(taylor_dtm, k = 3, control = list(seed = 1234))

#STEP 5 - look at beta values - word distribution per topic
taylor_topics <- tidy(taylor_lda, matrix = "beta")

#5B get the top ten per topic
taylor_top_terms <- taylor_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#5C visualize those words
taylor_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#STEP 6: look at the topic to topic spread (assumes a two topic comp)
beta_spread <- taylor_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#6B visualize that
beta_spread%>%
  filter(topic2/topic1 > 5)%>%
  ggplot(aes(topic2/topic1, term, colour=topic3))+geom_jitter()

#STEP 7 - assign topic guess values...
taylor_documents <- tidy(taylor_lda, matrix = "gamma")

#7B plot the results 
ggplot(taylor_documents, aes(document, topic, colour=gamma, size=gamma))+geom_point()

#Step 8 join back to the data
View(taylor_5)
taylor_lines