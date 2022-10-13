#core skills that you learned this unit

#we can access bundles of code that are already written with libaries
library(dplyr)

#we run each line and can look down in the console to see the effect
library(nycflights13)

#but flights is invisible until you assign it a name
flights<-flights

#arguments go left to right 
#bigger to smaller; abstract to specific
#and pipes let you pass the RIGHT HAND SIDE of the line to the LEFT hand side of the next line

#we can check in our file environment for uploading or downloading files
readr::TV <- read_csv("TV.csv")

#we can look inside in a few ways
#wide
glimpse(TV)
#long
head(TV)

#wide and long are two very different ways that our brains work
#sometimes we need to transition between them
#wide data are especially important because their dual encoding (both axes have information)
#so they let us build rich stories, but are bad for analysis and visualization

#we can look around in things
TV %>% 
  filter(Network == "ABC")

#or arrange our data quickly (much faster than excel)
TV %>% 
  arrange(desc(Rating))

#we can query with multiple factors and arrange
TV %>% 
  filter(Network=="NBC" & Type == "Sitcom" & Rating > 20) %>% 
  arrange(desc(Rating))

#we can also rename things
#destructively
colnames(TV)[3]<-"Place"
#and mostly reverse it with tidy
TV<-TV %>% mutate("Rank" = Place)

#We can also quickly summarize too...
TV %>% 
  group_by(Year,Network) %>% 
  summarize(Rating=mean(Rating))
#which we can store
A<-TV %>% 
  group_by(Year,Network) %>% 
  summarize(Rating=mean(Rating))

#and make graphics of
ggplot(A, aes(Year,Rating,colour=Rating))+geom_jitter()
#call your DATASET
#establish your aesthetic bindings (X,Y,Colour,Size,Alpha)
#add your mark (geom)
#then facets (multiples)
#and styles

#there are MANY more geoms that can be used, our group wrote a package that uses PICTURES as geoms


#all done up...
ggplot(A, aes(Year,Rating,colour=Rating))+geom_jitter()+facet_grid(~Network)+
  scale_color_gradient(low="purple", high="yellow")+theme(axis.text.x = element_text(angle = 90))

#we can also add a fun new data dimension...
#import joiner

#two columns with the SAME NAME, store that
B<-inner_join(A, joiner)

#joins are very useful and can be used to filter too
#the math on the BACK of the DPLYR cheatsheet is called 'set theory'

#make more graphs
ggplot(B, aes(Year,Rating,colour=Genre))+geom_jitter()+facet_grid(~Network)+
  scale_color_discrete()+theme(axis.text.x = element_text(angle = 90))+labs(title="What Music Was Popular?")

#these skills allow you to make maps or almost anything...

