#mini-lecture - telling datastories
#PDF on canvas
#Let's tell a datastory with the TV data...
#groups - we have 15 minutes

#clorpleth maps are really just a join and then a GGPLOT
library(maps)
library(mapdata)
library(ggplot2)
library(dplyr)

#let's call our data 
states <- map_data("state")
head(states)
#these are just a bunch of points, almost like a map is just a bunch of polygons...

#we just run a basic ggplot but with the 
ggplot(states, aes(x = long, y = lat, fill = region, group = group)) + 
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  #this takes of the list of 50 colors, the warning is that it is an old code
  #but it checks out
  guides(fill=FALSE)

#wait a second, what if we just wanted to see like a few states?
#graph the west_coast
ggplot(data = west_coast) + 
  #this is OLD style, you can put those geom controls inside the ggplot these days
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)


#NEW CODE

#THIS IS BASE-R IT IS DESTRUCTIVE
colnames(elector)[1]<-"region"
#FIX the ?
colnames(elector)[?]<-"votes"

#try this
states_map <- map_data("state")
ggplot(elector, aes(map_id = region)) +
  geom_map(aes(fill = Value), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)

#the fixed one...
ggplot(elector, aes(map_id = region)) +
  geom_map(aes(fill = ?), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat)+
  facet_wrap(~Value)

#map of oregon counties
oregon_map<-map_data('county', 'oregon')

ggplot(oregon_map, aes(x = long, y = lat, fill = region, group = group)) + 
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  #this takes of the list of 50 colors, the warning is that it is an old code
  #but it checks out
  guides(fill=FALSE)

#50 maps of the united states...
ggplot(states_map, aes(x = long, y = lat, fill = region, group = group)) + 
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  #this takes of the list of 50 colors, the warning is that it is an old code
  #but it checks out
  guides(fill=FALSE)+facet_wrap(~region)

#can we group things...
#in groups - add a grouper...

#Let's see some goodness...

