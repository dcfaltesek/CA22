#overview of how to do a basic network in R
#gephi can be really fun, but some of you aren't packing the hardware 
#or you might want to have more easily reproducable social science

#step one: get your dataset into a network; the football data is really good
library(sna)
library(network)
library(ggplot2)
library(dplyr)
library(ggnetwork)

#step oneB: run the football program

#step oneC: create the network
#we are running directed=false today so all our measures work nicely
foot<-network(clean_football, directed=FALSE,  matrix.type = "edgelist")

#step two: run your calculations (just like on gephi)
#this code looks different from GGPLOT and DPLYR because it is REALLY OLD
#none of this changed since 2004. Why? because it works and because mathematical innovation in this space is very slow
set.vertex.attribute(foot, "between", betweenness(foot))
set.vertex.attribute(foot, "close", closeness(foot))
set.vertex.attribute(foot, "count", degree(foot))
set.vertex.attribute(foot, "eigen", evcent(foot))
set.vertex.attribute(foot, "stress", stresscent(foot))

#step two b: take a look at a dataframe with your vertex measurements
#this code NEVER CHANGES
measures<-data.frame(get.vertex.attribute(foot, "vertex.names"),between=get.vertex.attribute(foot, "between"), 
                     close=get.vertex.attribute(foot, "close"), degree=get.vertex.attribute(foot, "count"), 
                     eigen=get.vertex.attribute(foot, "eigen"), stress=get.vertex.attribute(foot, "stress"))


#analytical note, now that we are in the heart of the season the network will get cleaner every week
#to see a general sense of the network and how much information flows
gtrans(foot)

#lets compare that to some random nonsense shall we?
g<-rgraph(50)
gtrans(g)

#what does that mean?
#college football has way to short a season to make meaingful inferences

#step three: make a quick plot so you can see that you are on the right track
#we can control our aesthetiecs using ggnetwork
n<-ggnetwork(foot, layout = "fruchtermanreingold", area = 1000, cool=2)

#step three b: the ggnetwork code is almost always identical
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes()) +
  geom_nodes(aes(size = between, alpha = count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")

#but what if we want to add some stuff that like, isn't in the original data
#step four: add some additional information

#as long as you have a dataset where the vertex names are the same, you can directly add node attributres
#step four b: import conf_data
set.vertex.attribute(foot, "state", conf_data$State.1.)
set.vertex.attribute(foot, "enrollment", conf_data$Enrollment)


#BECAUSE WE LOVE TO PARTY...
#step four c: let's add an edge attribute too.
#this data for edges was automatically created by our football program
set.edge.attribute(foot, "day", foot_meta$Day)

#step five: let's get spicy with a really rich feature full graph
#step five a: RENDER IT!
n<-ggnetwork(foot, layout = "kamadakawai", area = 1000, cool=2)

#base render code
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  #edges by state where the team is from and the day the game was played
  geom_edges(aes(colour=state, linetype=day)) +
  #and lets size them based on between and alpha them based on ineverse games played
  geom_nodes(aes(size = between, alpha = 1/count)) +
  #oh label them too
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  #and lets put a legend at the bottom because why not?
  theme(legend.position = "bottom")

#here is an EASY WAY TO CLEAN AND PIPE WHEN ITS MESSY
#step five b: let's get rid of Idaho.
n %>% 
  #only teams that have played five games allowed. 
  filter(count>10) %>% 
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  #edges by state where the team is from and the day the game was played
  geom_edges(aes(colour=state, linetype=day)) +
  #and lets size them based on between and alpha them based on ineverse games played
  geom_nodes(aes(size = between, alpha = 1/count)) +
  #oh label them too
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  #and lets put a legend at the bottom because why not?
  theme(legend.position = "bottom")

#PROTIP: AFTER YOU RENDER IT, all your dplyr and ggplot tricks are in FULL-EFFECT!

#step six: detect communities automatically, we will do like three methods

#to calculate louvain (like gephi), two lines
#some of these methods prefer matrix inputs to edgelist inputs
smasher<-as.matrix.network.adjacency(foot)
louvain_net<-modMax::greedy(smasher)

set.vertex.attribute(foot, "louvain", louvain_net$`community structure`)

#and kcores
kc<-kcores(foot)
set.vertex.attribute(foot, "kcores", kc)
#notice that this was MUCH FASTER

#for your analytical enjoyment
mods<-data.frame(vertex.names=get.vertex.attribute(foot, "vertex.names"), louvain_net, kc)
mods<-inner_join(mods, conf_data)
#lets have a look!
View(mods)

#we can now use those in our graphs
# WE MUST RERENDER!
n<-ggnetwork(foot, layout = "springrepulse")
n %>% 
  #only teams that have played five games allowed. 
  filter(count>14) %>% 
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  #edges by state where the team is from and the day the game was played
  geom_edges(aes(linetype=day)) +
  #and lets size them based on between and alpha them based on ineverse games played
  geom_nodes(aes(size = between*5, colour=as.factor(louvain),alpha = 1/count)) +
  theme_blank() +
  #and lets put a legend at the bottom because why not?
  theme(legend.position = "bottom")

#if you have gotten this far, you will be fine. 

