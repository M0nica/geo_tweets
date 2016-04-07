# server.R

setwd("~/Documents/geo_tweets")
library(streamR)
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)
library(ggplot2)
source("parseR.R")
#source("lecture08.R")

setwd("~/Documents/geo_tweets")
library(streamR)
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)
library(ggplot2)
#geo_tweets02.09.2016 = parseTweets("loc_tweets.02.09.2016.json")
geo_tweets = parseTweets("loc_tweets.03.01.2016.json")


#files <-c("loc_tweets.02.09.2016.json", "loc_tweets.02.20.2016.json", "loc_tweets.02.23.2016.json", "loc_tweets.02.27.2016.json", "loc_tweets.03.01.2016.json","loc_tweets.03.05.2016.json", "loc_tweets.03.06.2016.json", "loc_tweets.03.08.2016.json", "loc_tweets.03.15.2016.json") 



# geo_tweets = parseTweets(filename)
tw_coordinates_B<- cbind(geo_tweets$lon,geo_tweets$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
tw_points_B <- SpatialPoints(tw_coordinates_B2)
plot(tw_points_B)

#proj4string(tw_points_B) <- crs.geo
#crs.geo <- CRS("+init=EPSG:32633")

plot(tw_points_B)
class(tw_points_B)

all_states <- map_data("state")
plot(all_states)

library(rgeos)
library(GISTools)

require(maps)
usa <- map("state", fill = TRUE)

require(sp)
require(maptools)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

poly.counts(tw_points_B, usa)


US_states <- map_data("state")
#geo_tweets = parseTweets("loc_tweets.03.01.2016.json")
#geo_tweets = parseTweets("loc_tweets.02.09.2016.json")


##generate U.S. map
ggplot()+ geom_polygon( data=US_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo_tweets, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "blue")



# 
# 
# usa <- map("state", fill = TRUE)
# 
# 
# geo_tweets = parseTweets("loc_tweets.03.01.2016.json")
# #mydata = data.frame(cbind(BernieTotals, DonaldTotals, HillaryTotals, MarcoTotals, TedTotals))
# #names = c("Bernie Sanders", "Donald Trump", "Hillary Clinton", "Marco Rubio", "Ted Cruz")
# 
# 
# tw_coordinates_B<- cbind(geo_tweets$lon,geo_tweets$lat)
# tw_coordinates_B2 <- na.omit(tw_coordinates_B)
# tw_points_B <- SpatialPoints(tw_coordinates_B2)
# #plot(tw_points_B)
# 
# #proj4string(tw_points_B) <- crs.geo
# #crs.geo <- CRS("+init=EPSG:32633")
# 
# plot(tw_points_B)
# class(tw_points_B)
# 
# all_states <- map_data("state")
# plot(all_states)
# 
# #library(rgeos)
# #library(GISTools)
# 
# #require(maps)
# #usa <- map("state", fill = TRUE)
# 
# 
# # require(sp)
# #require(maptools)
# IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
# usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
# 
# poly.counts(tw_points_B, usa)
# 
# 
# US_states <- map_data("state")
# geo_tweets = parseTweets("loc_tweets.03.01.2016.json")
# 
# #geo_tweets = parseTweets("loc_tweets.02.09.2016.json")
# 
# 
# geo_tweets <<- geo_tweets
# plotMap <- ggplot()+ geom_polygon( data=US_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
# 
# plotTweets <- ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo_tweets, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "blue")

# 
shinyServer(function(input, output) {
})