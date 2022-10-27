install.packages("latticeExtra")
install.packages("gridExtra")
install.packages("Hmisc")
install.packages("reshape2")
install.packages("qgraph")
install.packages("IsingFit")
install.packages("NetworkToolbox")
install.packages("modMax")
install.packages("leiden")
install.packages("reticulate")

library(NetworkToolbox)

C<-network(g, directed=FALSE)
#to access some of this we need a matrix method
smasher<-as.matrix.network.adjacency(C)
#modularities
#louvain- this is a poor implementation of Louvain, but it works with the libraries
louvain_net<-modMax::greedy(smasher)
#kcores - older method of modularity, falls back to degree centrality 
kcores_net<-kcores(C)
#a method that implements CNM that refines based on commmonly placed nodes
msgvm_net<-modMax::msgvm(smasher, initial=c("general"), parL=50)
#coarsening levels
mome_net<-modMax::mome(smasher)

#cleans up that dataframe
modularities<-data.frame(louvain_net[3], kcores_net, msgvm_net[3], mome_net[3])
colnames(modularities)[1]<-"Louvain"
colnames(modularities)[4]<-"msgvm"
colnames(modularities)[5]<-"mome"
View(modularities)

C %v% "louvain" <- louvain_net[3]
C %v% "kcores" <- kcores_net
L<-ggnetwork(C, layout = "fruchtermanreingold")
J<-filter(L, vertex.names>75)

ggplot(J, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(aes(linetype = TYPE))+
  #set the other aesthetics as static values
  geom_nodetext(aes(colour = as.factor(kcores), label = vertex.names))+
  theme_blank()

#further fun
cutpoints(C)
mutuality(C)
court_brokerage<-brokerage(C, modularities$Louvain)

#strengths - larger numbers mean stronger
comm.str(smasher, modularities$Louvain)
comcat(smasher)
impact(smasher)
leverage(smasher)

#let's redo football

#feeeet ball

#import conf_clean - this now joins smoothly for all team data about FCS and FBS ball
#foot_meta is in memory - this is scores, dates, times, etc...
#clean_football is a pure edge list
clean_football<-clean_football[-495,]
footy<-network(clean_football, directed=TRUE, matrix.type="edgelist")

#now we want to add some information about teams
#these are node properties

#these libraries are really sensitive to ordering - so join now
intermediate<-get.vertex.attribute(footy, "vertex.names")
advanced<-data.frame(vertex.names=intermediate)
conference_clean<-inner_join(advanced, conf_data)


footy%v%"conference"<-conference_clean$CurrentConference
footy %v% "nickname" <- conference_clean$Nickname
footy %v% "size" <- conference_clean$Enrollment
footy %v% "nickname" <- conference_clean$Level
footy %v% "age" <- conference_clean$FirstPlayed
footy %v% "state" <- conference_clean$State.1.

#it's edge time
set.edge.attribute(footy, attrname = "score", (as.numeric(full_data$Pts)-as.numeric(full_data$Pts.1)))
set.edge.attribute(footy, attrname = "date", full_data$Date)
set.edge.attribute(footy, attrname = "time", full_data$Time)
set.edge.attribute(footy, attrname = "day", full_data$Day)

library(ggnetwork)
#this one is just supposed to be pretty
ggplot(footy, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = date)) +
  geom_nodes(aes(colour = "blue", size=5)) +
  geom_nodetext_repel(aes(label = vertex.names))


