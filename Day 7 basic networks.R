#the basics
library(ggplot2)
library(dplyr)
#social network analysis
library(sna)
#a gg version of networks
library(ggnetwork)

#you are also going to need a network...
#a source of arbitrary data
#source an entire file called football_extract.R

#look at clean_football
#what do you notice, write your notes HERE:



#import the edge list as a network, this will accept ANY two columns 
n<-network(clean_football, directed = TRUE, matrix.type = "edgelist")

#network data can also be stored as a matrix
as.sociomatrix(n)

#how many connections...
network.edgecount(n) 

#cool, so do you think 488 games is enough to like, know things about stuff?

#get the names and the number of games played...
game_list<-data.frame("Team"=network.vertex.names(n),"GamesPlayed"=degree(n))
ggplot(game_list, aes(GamesPlayed))+geom_histogram()
#wait, wut?

#how mant teams have played fewer than four games...
game_list %>% 
  filter(GamesPlayed<4) %>% 
  summarize(sum(GamesPlayed))

#so of 488 college feetball games to this point 113 have been meaningless?
113/488

#so we have 131 schools connected by
(488-113)/131

#do we know much about feetball then?

#lets make a graphic, mmmk?
#SNA IS NOT TIDY! WHEN YOU RUN CODE IT DOES STUFF.
#that just means you have to reverse a few steps, for some of you, this will make more sense

set.vertex.attribute(n, "games", degree(n))

#when you make a network - DONT CHANGE THE FIRST LINE
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = games)) +
  #and always run theme blank
  theme_blank()

#you are now beyond google sheets and excel, welcome to the undiscovered country. 

#and with more style...
ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "pink") +
  geom_nodes(aes(size = count, color = as.factor(count))) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank()+
  theme(legend.position = "bottom")

#for our first by styling choice, lets take a step back to a simple graph
K<-ggnetwork(n, layout = "kamadakawai")
L<-ggnetwork(n, layout = "circrand")
M<-ggnetwork(n, layout = "geodist")
N<-ggnetwork(n, layout = "hall")
O<-ggnetwork(n, layout = "mds")
P<-ggnetwork(n, layout = "target")
Q<-ggnetwork(n, layout = "princoord")
R<-ggnetwork(n, layout = "random")
S<-ggnetwork(n, layout = "rmds")
U<-ggnetwork(n, layout = "segeo")
V<-ggnetwork(n, layout = "seham")
W<-ggnetwork(n, layout = "spring")
B<-ggnetwork(n, layout = "springrepulse")
C<-ggnetwork(n, layout = "eigen")

#replace XXXXXX with a leter from above, each of these can be controlled as well...
ggplot(XXXXXXX, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = games)) +
  #and always run theme blank
  theme_blank()

#to learn how to control settings, you can use
??gplot.layout
#or whatever SNA layout you like

#you can jitter the nodes, 
FF<-ggnetwork(n, layout = "fruchtermanreingold", cell.jitter=1, repulse.rad=1)
ggplot(FF, aes(x, y, xend = xend, yend = yend)) +
  #change the GEOMS
  geom_edges(color = "orange") +
  geom_nodes(aes(color = games)) +
  #and always run theme blank
  theme_blank()
#play with the repluse and you can see some stuff..

#next time, centralities, then modularities...


