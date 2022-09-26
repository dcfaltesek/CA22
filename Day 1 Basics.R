#if you are reading this, it means you connected to the github. Great Job!

#this is an integrated development environment. It has four panes (clockwise from this one): scripts, environment, files, and console
#in the environment pane, you can see what has been loaded
#in the files pane you can look for files in your directory, plots, packages, or helpfiles (this is also where you can upload and download)
#the console shows what has actually run, RED is NOT A BAD COLOR

#a few key notes, you need to execute every line or group of lines. Here are some examples

2+2

#you can execute this by  hitting command enter or apple enter at the end of the line.
#you could also highlight it and press run in the upper right of this panel

#often for cool stuff, things will need more than one line, consider this function
#besure to run the ENTIRE function (which ends on line 20)
badhabit<-function(x){
  y<-x+2
  print(y)
}

#let's run it...
badhabit(7)

#functions always take one argument in on the LEFT SIDE and put out a result from the RIGHT SIDE

#now we can try some stuff; this will be the hard one for you to follow
library(nycflights13)

#We then could access the data by 
flights

#and to make our lives easier we can store it as a variable in our global environment
flights <- flights

#the arrow is the key, use that any time you want to add something to the global environment

#NYC flights has other stuff too
nycflights13::airlines
nycflights13::airports
nycflights13::planes
nycflights13::weather

#notice up there how there are :: ?
#those are how we call something WITHIN a package if we don't call it as a library
#in this case we already called nycflights, I just used :: to help us learn

#let's start with some basic dplyr stuff
library(dplyr)
#most people pronounce this de-plier, as in give me de pliers
#I often say dp-lyr like Doppeler 

#so look up the birthday of america
filter(flights, month ==7 & day ==4)
#this lookup had THREEE arguments, ONE on the left of the comma, two on the right
#this is a key idea - each function has a right and a left side

#BOOLEANS are your friend 
#what if we want flights from Newark OR JFK? Note the ==
filter(flights, origin == "EWR" | origin == "JFK")

#we can switch the OR bar for an &
filter(flights, origin == "EWR" & dest == "PDX")

#does not equal
filter(flights, month != 2)

#so let's make a fun pretty graphic with all the fixins'
library(ggplot2)
flights %>% 
  filter(month==9) %>% 
  ggplot(aes(air_time, dep_delay, colour=carrier))+geom_jitter()+facet_grid(~origin)+labs(x="Air Time", y="Departure Delay", title="Such Delays", colour="Carrier")

#here are a few more handy functions: 

#we just want the top ten most delayed flights
slice_max(flights, dep_delay, n=10)

#we just want the top 1% of arrival delays
slice_max(flights, arr_delay, prop = .01)

#slice min does the other thing, if you want ties 
slice_min(flights, arr_delay, prop=.01, with_ties=TRUE)

#make me a graphic of the top 500 delayed flights (WE)
late<-slice_max(flights, dep_delay, n= 500)

#Now lets have a few races...


#Let's do this together
#let's find the most delayed flight in October 
#how many flights on your birthday
#lets do a few more

#let's do a faceted graph from your birthday...


#NOW we mess with the cocaine data
ggvis::cocaine

#the :: speciically calls something within a package sort of like $
cocaine<-ggvis::cocaine

#your challenge - make a plot where potency and price are the axes