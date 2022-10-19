#scraping for fun and profit
#you need data, right?
#websites have data, those jerks, we want it.
library(rvest)

#BUT I NEED A LIST OF EVERY COUNTRY WITH MCDONALDS NOW NOW NOW!!!!!!
restaurant<-"https://en.wikipedia.org/wiki/List_of_countries_with_McDonald%27s_restaurants"

food<-restaurant%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='/html/body/div[3]/div[3]/div[5]/div[1]/table[2]')%>%
  html_table(header = TRUE)

coffee<-"https://www.silverdoorapartments.com/blog/which-country-has-the-most-starbucks-per-1000000-inhabitants/"

#parse the football data
latte<-coffee%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_elements(css = "#sda-blog-post-content > table:nth-child(5)")%>%
  html_table(header = TRUE)

#here is a problem - the data is all DIRTY, you need to clean it