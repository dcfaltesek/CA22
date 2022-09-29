#Day Two - confirming our filter knowledge
#Learning Outcomes
#Confirm basic dplyr knowldege
#Produce plots quickly using ggplot2
#Use a pipe to filter a full dataset 
#Add labels 
#map colors for discrete and continuous
#summarize a more complex dataset

#First how do you load a library
library(dplyr)
library(ggplot2)

#let's start with some new data... the weddings dataset
#we need to load that in our Environment pane

#you can summarize this with a function called head()
head(weddings)
#this data is tidy, meaning each column is a variable and each row is a case
#which columns are discrete?

#let's do some basic grammar of graphics
#we define the dataset, then the aesthetics, then the geom
ggplot(weddings)

#THAT GRAPH PRODUCED: 

ggplot(weddings, aes(Budget, Guests, colour=State))

#THAT GRAPH PRODUCED:

ggplot(weddings, aes(Budget, Guests, colour=State))+geom_jitter()

#AND BECAUSE WE CAN...look at that, we are doing some MATH INSIDE THE FUNCTION
ggplot(weddings, aes(Budget, Guests, colour=State, size = Dress+Venue+Experience))+geom_jitter()

#Let's write a new one with a discrete X and Y axis
ggplot(weddings, aes(Result, Budget.1, colour=Experience))+geom_jitter()

#let's play around for a minute, which other GEOMS will be awesome here...

#Now for a research design minute, let's test a hypothesis. If the bride is OLDER the EXPERIENCE will be lower, because they can't party hard.
cor.test(weddings$Age, weddings$Experience)
#how to interpret; pvalue <.05; coefficient

#remember, you are just rejecting the null hypothesis
#what we are doing here is exploratory data science


#sometimes we might want to combine some things, that is when we use a pipe
#you can make a pipe with apple-shift-m
#what if we wanted to get average budget by state?

#start BIG with the dataset
weddings %>% 
  #which is the left argument for
  group_by(State) %>% 
  #those groups are now the left argument for
  summarize(mean(Budget))
  

#OH fancy, now we can add a graph to that

weddings %>% 
  group_by(State) %>% 
  #to make it easier we will NAME the summary
  summarize(Average=mean(Budget)) %>% 
  #this new happy dataset (States and Average)
  ggplot(aes(State, Average))+geom_point() + 
  #lets turn the labels 90 degrees
  theme(axis.text.x = element_text(angle = 90))


#Let's get in some groups and talk about this data, find some trends, what happened here
#how do you win?


#now lets make some stuff pretty, why not?
library(tvthemes)
library(ggthemes)
ggplot(weddings, aes(Budget.1, Result, colour=Experience))+geom_jitter()+scale_color_brooklyn99(type="continuous")

#but how many TV themes do we have to play with...
??tvthemes

ggplot(weddings, aes(Budget.1, Result, colour=Experience))+geom_jitter()+scale_color_simpsons(type="continuous")

#but Simpsons if it was 1993
ggplot(weddings, aes(Budget.1, Result, colour=Experience))+geom_jitter()+scale_color_simpsons(type="continuous")+theme_excel()

#now give me Nate Silver but with his friend Ron Swanson
ggplot(weddings, aes(Budget.1, Result, colour=Experience))+geom_jitter()+scale_color_parksAndRec(type="continuous")+theme_fivethirtyeight()

#lets just have fun for a second
ggplot(weddings, aes(State, Budget, size = 1/Result, colour=Experience))+
  geom_jitter()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_gradient(low="green",high="darkgreen")+
  labs(title="When More is Less")


#let's try it again
#load the weddings data...
glimpse(weddings)

#this dataset has everything, categorial, discrete, location, names, and even different levels of measure

#somethings here are dirty and hard, lets clean them

#try this
weddings%>%
  #new name first
  rename(Bride.Name = Bride.1)
  
colnames(weddings)[1]<-"Dan is Cool"

#that makes more sense
#so which one matters most to victory?

#we need a new column with the total score
w2<-weddings%>%
  mutate(Total = Dress+Venue+Food+Experience)

#now we can use w2

#now change that up a litte... take five minutes and chat with your pals...

#Now we need to learn some more advanced dplyr stuff, and for this you will need to use pipes
#what if we want to know some averages
weddings%>%
  group_by(Result)%>%
  summarize(mean(Experience), mean(Dress), mean(Food), mean(Venue))


