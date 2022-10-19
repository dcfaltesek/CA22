#Core Skills for Unit One
#import a dataset


#install a package 
install.packages("nycflights13")

#attach a package
library(nycflights13)

#import relevant packages for your work
library(dplyr)
library(ggplot2)

#Write a filter
#just flag day
flights%>%
  filter(day==14)%>%
  filter(month==6)

#there are multiple right answers here...


#Use select to make a table managible
flights%>%
  select(origin, dest)

#Write a summary function 
flights%>%
  group_by(carrier)%>%
  summarize(mean(dep_delay, na.rm= TRUE))

#calculate something across a window
explain<-flights%>%
  mutate("lagged_delay"=arr_delay-dep_delay)%>%
  select(time_hour, lagged_delay,carrier, arr_delay, dep_delay)

View(explain)

#plot that
ggplot(explain, aes(time_hour, lagged_delay, colour=dep_delay))+geom_point()+

#check to see if two things are related...
cor.test(flights$arr_delay, flights$dep_delay)
#Turns out they are, also that is significant

#Produce an appropriate discrete plot
z<-ggplot(flights, aes(carrier, dep_delay, colour=origin))+geom_boxplot()

#easy flips and such
z+coord_polar()+facet_grid(~origin)
z+coord_flip()

#produce an appropriate continuous plot
ggplot(flights, aes(dep_time))+geom_freqpoly()+facet_grid(~origin)

#demonstrate control of advanced plot aesthetics including color, labels
flights%>%
  filter(hour>20)%>%
  ggplot(aes(time_hour, dep_delay, group=carrier)) +
  geom_line(aes(linetype="dotted", color=carrier, size=2))+
  geom_point(aes(color=carrier, size=3))+
  scale_colour_brewer(palette = "Greens")+
  facet_grid(~origin)+
  ggtitle("We Only Come Out At Night")+xlab("when")

#and you need to export stuff
write.csv(explain, "explaining_delays.csv", row.names = FALSE)

