#OK so I just gotta know, what do you think of the green ranger?

#go ahead an import dataset zords
zords

#Someone quick use the Dragon Dagger!

#ok can you make a GGPLOT OF IT? LIKE BY SEASON?


Seasons<-c("Dino", "Thunder", "Ninja","Shogun", "Zeo", "Aquatar", "SuperZeo")
Number<-c(1,2,3,4,5,6,7)
ranger_seasons<-data.frame(Seasons, Number)

#but this isn't easy at all
head(zords)

#that is because it is WIDE data, which is really nice for a human brain to look at
#computers need LONG data

library(tidyr)
zords %>% 
  #first step - we need to take everything EXCEPT the first column and make it LONGER
  pivot_longer(-Ranger)

#Well that was awesome, but it is missing some things, we need to get it to say the right stuff for name and value
zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord")

#well that was easy
library(dplyr)
long_rangers<-zords %>% 
  pivot_longer(-Ranger, names_to="Seasons", values_to="Zord") 

inner_join(long_rangers, ranger_seasons, by="Seasons")

#so now we can plot that nonsense...
library(ggplot2)
inner_join(long_rangers, ranger_seasons) %>% 
  ggplot(aes(Number, Zord, colour=Ranger))+geom_point()

#OK I WANT TO NAME NAMES, let's make a plot of flights that actually says which airline screwed up. It is group time....



#lets get into some datastories... 
TV %>% 
  group_by(Year, Network) %>% 
  summarize(Rating=mean(Rating)) %>% 
  pivot_wider(names_from = Network, values_from = Rating)

#now we can do some plotting and stuff right?
#yeah no, but this makes a really nice table to look at


table<-TV %>% 
  group_by(Year, Network) %>% 
  summarize(Rating=mean(Rating)) %>% 
  pivot_wider(names_from = Network, values_from = Rating)
View(table)

#an actual plot is really straight forward
ggplot(TV, aes(Year, Rating, colour=Type))+geom_jitter()

