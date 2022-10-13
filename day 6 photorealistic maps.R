#missing step - log into google...
#some steps will work without it...
geocode("waco, texas")
ggmap_show_api_key()

#useful metadata
getOption("ggmap")

#google will return all sorts of fun things for you
a<-geocode("Univeristy of Hawaii")

#we can also get data for a number of strings...
B<-c("University of Minnesota", "University of Wisconsin,", "University of Texas")
geocode(B)

C<-geocode("United States")
D<-get_map(C)
ggmap(D)

G<-get_googlemap(center=c(lon= -95, lat = 29), zoom = 10, scale=2)
ggmap(G)


#donuts example
PDX<-geocode("PDX")
PDX_map<-get_map(PDX)
PDX_map
?get_googlemap

ggmap(PDX_map, extent = "device") +
  geom_point(aes(x = lon, y = lat, colour = flavor), data = donut, alpha = .5)+scale_colour_gradient(high="red",low='green')

PDX_map_google<-get_googlemap(center = c(lon=-123, lat=45.5), zoom=10, maptype = c("satellite"))

ggmap(PDX_map_google, extent = "device") +
  geom_point(aes(x = lon, y = lat, colour = flavor), data = donut, alpha = .5)+scale_colour_gradient(high="red",low='green')



