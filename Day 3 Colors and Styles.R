#day three: advanced aesthetics

#three tasks for day three
#A. get good at making stuff pretty
#B. get better at filters and summaries
#C. be ready to do joins next time

#Let's store a simple discrete continuous from weddings
library(ggplot2)
#notice, no geom is attached here...
W<-ggplot(weddings, aes(State, Budget, colour=Result))

#let's do this the wrong way...
W+geom_jitter()

#ANSWER FOR YOURSELF: why is jitter wrong here?

#Let's do it the right way...
W+geom_violin()

#Fixing labels

W+labs(title="This just in, I am cool!")+theme(axis.text = element_text(size = 14), axis.text.x = element_text(angle = 45),axis.text.y = element_text(angle = -45))
#how do I find all the things I can change?
??ggplot2
#once you are in the help 


#Colors, first continuous colors
ggplot(weddings, aes(x = State, y = Experience, fill=Experience))+geom_bar(stat = "identity")+scale_fill_continuous_diverging()

#let's make if fancy...
ggplot(weddings, aes(Age, Budget)) +
  geom_point(aes(colour = as.factor(Result)))+
  scale_colour_brewer(palette = "Greens")+facet_wrap(~State)

ggplot(weddings, aes(Age, Budget, colour=as.factor(Result))) +
  geom_point()+
  scale_colour_brewer(palette = "Greens")+facet_wrap(~State)

#but make it continous and beavery
ggplot(weddings, aes(Age, Result, colour=Budget)) +
  geom_point()+
  scale_colour_distiller(palette = "Oranges")+facet_wrap(~State)

#once again forcing the discrete...
ggplot(weddings, aes(x = Budget, fill = as.factor(Result))) +
  geom_histogram(position = "dodge", binwidth = 1000)

p<-ggplot(weddings, aes(x = Budget, fill = as.factor(Result))) +
  geom_histogram(position = "dodge", binwidth = 5)

p+scale_fill_brewer(palette = "Oranges",direction = -1) +
  theme_dark()

#we need to get a handle on summary functions
#what if we wanted to count all the weddings in each state?

#first we need our handy data pliers...
library(dplyr)
#name the dataset
weddings %>% 
  #how should we group
  group_by(State) %>% 
  #our summary function
  count()

#can you write a group that counts how many brides were of each age?



#what if we want to see the five most expensive weddings?
weddings %>% 
  slice_max(Budget, n=5)

#how about 20 random weddings
weddings %>% 
  slice_sample(n=20)

#the manipulate area is pretty clear 

#what if we just wanted to sort...
weddings %>% 
  arrange(desc(Episode))

#just a classic summary function 
weddings %>% 
  group_by(Age) %>% 
  summarize(Average_Spend=mean(Budget)) %>% 
  ggplot(aes(Age, Average_Spend))+geom_density_2d_filled()


#Now we start to think about joins...
#Let's make an attacher

#Three attributes
State<-c("New Jersey","New York","New Hampshire")
Things<-c("Bad Drivers", "Bad Pizza", "Wut")

#make those into a happy little dataframe
attacher<-data.frame(State, Things)
attacher

#now here is the magic...
new_weddings<-inner_join(weddings, attacher)




