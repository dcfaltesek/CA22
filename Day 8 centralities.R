#library list
library(sna)
library(network)
library(ggplot2)
library(dplyr)
library(ggnetwork)

#The Road...
#produce a network
R<-network(roads, directed = FALSE, matrix.type = "edgelist")
#node attributes
set.vertex.attribute(R, "between", betweenness(R))
set.vertex.attribute(R, "close", closeness(R))
set.vertex.attribute(R, "count", degree(R))

#about that edge
set.edge.attribute(R, "where", roads$WHERE)

#eigenvector based layourt using the strong assumptions, you can switch to weak if you want
K<-ggnetwork(R, layout = "eigen", "symstrong")

#area is size, cool controls Simulated Annealing - bigger numbers bigger crystals 
P<-ggnetwork(R, layout = "fruchtermanreingold", area = 100, cool=2)

#a physical simulation of a spring process - kilograms per newton meter 
#mass is in KG - heavy like bowling balls
#equi is when springs and mass balance
#repe
#kfr is friction - I have it turned down....
#repulsive forces are turned on...
P<-ggnetwork(R, layout = "spring", mass=5, equil=.1, repeqdis=1, kfr = .001, repulse=TRUE)

#highway plots
ggplot(K, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(colour=where)) +
  geom_nodes(aes(size = between, alpha = 1/count)) +
  geom_nodetext_repel(aes(label = vertex.names))+
  theme_blank() +
  theme(legend.position = "bottom")

#overall centralization
centralization(R, betweenness)

#lets make a table
highway_data<-data.frame(
  road=get.vertex.attribute(R, "vertex.names"),
  degree=degree(R, cmode="indegree"),
  between=betweenness(R),
  close=closeness(R),
  #eigenvector prestige, degree to which nominated
  prestige=prestige(R, cmode = "eigenvector", rescale=TRUE),
  #prestige by proximity weighted domain prestige
  prestige2=prestige(R, cmode = "domain.proximity", rescale=TRUE),
  #measure like bonacich power centrality
  information=infocent(R), 
  bonpow=bonpow(R),
  eigenvector=evcent(R),
  load=loadcent(R)
)

cutpoints(R)
maxflow(R)

#let's do it again but with Parks and Rec



