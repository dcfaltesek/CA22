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


