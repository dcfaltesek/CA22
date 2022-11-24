library(dplyr)
library(rsample)
library(tidyr)
library(ggplot2)
library(tidymodels)
library(parsnip)

#we can model this in a few ways, let's start by getting a sense of how rich this data is

songsC %>% 
  ggplot(aes(energy, acousticness, colour=tempo))+geom_jitter()+facet_wrap(~album_name)

#this is a pretty fun dataset

#what if we wanted to do what we have before with classifying it via text?
#we could try topic modeling, we could also use some fancier methods

#to help us reassemble things later, add a reference variable
songsD<-songsC %>% mutate("ref" = 1:dim(songsC[1]))


data_split <- songsD %>%
  initial_split(prop = .8)

#assign the split data to two categories
training_data <- training(data_split)
validation_data <- testing(data_split)

library(textrecipes)
rec <- recipe(album_name ~ song_lyric, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 1)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 500) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
learn_data <- bake(rec, new_data = training_data)

library(parsnip)
#accuracy is from forecast
library(forecast)
is4<-rand_forest("classification") %>%
  set_engine("ranger") %>%
  fit(album_name ~ ., data = learn_data) %>%
  predict(new_data = val_data) %>% 
  mutate(truth=validation_data$album_name)

ggplot(is4, aes(truth, .pred_class))+geom_point()


#what if we just didn't do that?
rec <- recipe(album_name ~ song_lyric+energy+key+loudness+mode+tempo, data = training_data) %>%
  #three deep ngrams
  step_tokenize(song_lyric, token = "ngrams", options = list(n = 1)) %>%
  #max tokens 250
  step_tokenfilter(song_lyric, max_tokens = 5063) %>%
  #tfidf method
  step_tfidf(song_lyric) %>%
  prep() 

train_data <- juice(rec)
val_data <- bake(rec, new_data = validation_data)
learn_data <- bake(rec, new_data = training_data)

library(parsnip)
#accuracy is from forecast
library(forecast)
is4<-rand_forest("classification") %>%
  set_engine("ranger") %>%
  fit(album_name ~ ., data = learn_data) %>%
  predict(new_data = val_data) %>% 
  mutate(truth=validation_data$album_name) %>% 
  mutate(correct=ifelse(.pred_class==truth,1,0))

ggplot(is4, aes(truth, .pred_class, colour=correct))+geom_jitter()
