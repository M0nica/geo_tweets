setwd("~/Documents/geo_tweets")
library(streamR)
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)
library(ggplot2)
library(grid)
source("parseR.R")


#geo_tweets02.09.2016 = parseTweets("loc_tweets.02.09.2016.json")
#geo_tweets = parseTweets("loc_tweets.03.01.2016.json")

# parse tweets

tweets.02.09.df <- parseTweets("loc_tweets.02.09.2016.json")
tweets.02.20.df <- parseTweets("loc_tweets.02.20.2016.json")
tweets.02.23.df <- parseTweets("loc_tweets.02.23.2016.json")
tweets.03.01.df <- parseTweets("loc_tweets.03.01.2016.json")
tweets.03.05.df <- parseTweets("loc_tweets.03.05.2016.json")
tweets.03.06.df <- parseTweets("loc_tweets.03.06.2016.json")
tweets.03.08.df <- parseTweets("loc_tweets.03.08.2016.json")
tweets.03.15.df <- parseTweets("loc_tweets.03.15.2016.json")

geo_tweets <- rbind(tweets.02.09.df, tweets.02.20.df, tweets.02.23.df, tweets.03.01.df, tweets.03.05.df, tweets.03.06.df, tweets.03.08.df, tweets.03.15.df)


#files <-c("loc_tweets.02.09.2016.json", "loc_tweets.02.20.2016.json", "loc_tweets.02.23.2016.json", "loc_tweets.02.27.2016.json", "loc_tweets.03.01.2016.json","loc_tweets.03.05.2016.json", "loc_tweets.03.06.2016.json", "loc_tweets.03.08.2016.json", "loc_tweets.03.15.2016.json") 

#layout(mat, widths = rep.int(1, ncol(mat)),
#      heights = rep.int(1, nrow(mat)), respect = FALSE)


# geo_tweets = parseTweets(filename)
tw_coordinates_B<- cbind(geo_tweets$lon,geo_tweets$lat,geo_tweets$place_lat,geo_tweets$place_lon)
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

HillaryClinton <- geo_tweets[grep("Hillary Clinton", geo_tweets$text), ]
HillaryClinton_wo <- geo_tweets[grep("HillaryClinton", geo_tweets$text), ]
HillaryTotals <- rbind(HillaryClinton, HillaryClinton_wo)
hc_coordinates_B<- cbind(HillaryTotals$lon,HillaryTotals$lat,HillaryTotals$place_lat,HillaryTotals$place_lon)
hc_coordinates_B2 <- na.omit(hc_coordinates_B)
hc_points_B <- SpatialPoints(hc_coordinates_B2)
plot(hc_points_B)
poly.counts(hc_points_B, usa)


#proj4string(tw_points_B) <- crs.geo
#crs.geo <- CRS("+init=EPSG:32633")

plot(tw_points_B)
class(tw_points_B)

poly.counts(tw_points_B, usa)

# 
# BernieSanders <- geo_tweets[grep("Bernie Sanders", geo_tweets$text), ]
# BernieSanders_wo <- geo_tweets[grep("BernieSanders", geo_tweets$text), ]
# BernieTotals <- rbind(BernieSanders, BernieSanders_wo)
# bs_coordinates_B<- cbind(BernieTotals$lon,BernieTotals$lat)
# bs_coordinates_B2 <- na.omit(bs_coordinates_B)
# bs_points_B <- SpatialPoints(bs_coordinates_B2)
# plot(bs_points_B)
# poly.counts(bs_points_B, usa)
# 
# DonaldTrump <- geo_tweets[grep("Donald Trump", geo_tweets$text), ]
# DonaldTrump_wo <- geo_tweets[grep("DonaldTrump", geo_tweets$text), ]
# DonaldTotals <- rbind(DonaldTrump, DonaldTrump_wo)
# dt_coordinates_B<- cbind(DonaldTotals$lon,DonaldTotals$lat)
# dt_coordinates_B2 <- na.omit(dt_coordinates_B)
# dt_points_B <- SpatialPoints(dt_coordinates_B2)
# plot(dt_points_B)
# poly.counts(dt_points_B, usa)
# 
# MarcoRubio <- geo_tweets[grep("Marco Rubio", geo_tweets$text), ]
# MarcoRubio_wo <- geo_tweets[grep("MarcoRubio", geo_tweets$text), ]
# MarcoTotals <- rbind(MarcoRubio, MarcoRubio_wo)
# mr_coordinates_B<- cbind(MarcoTotals$lon,MarcoTotals$lat)
# mr_coordinates_B2 <- na.omit(mr_coordinates_B)
# mr_points_B <- SpatialPoints(mr_coordinates_B2)
# plot(mr_points_B)
# poly.counts(mr_points_B, usa)
##generate U.S. map

#ggplot()+ geom_polygon( data=US_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )+ coord_cartesian(xlim = c(-180, 180), ylim = c(-90, 90))

#ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25)+ expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo_tweets, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "blue") +  geom_point(data = HillaryTotals, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "red")+  geom_point(data = BernieTotals, aes(x = lon, y = lat), size = 1, alpha = 3/5, color = "orange")

#p1 <-
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25)+ expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HillaryTotals, aes(x = lon, y = lat), size = 1.5, alpha = 1/5, color = "red")





#p2 <- ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25)+ expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DonaldTotals, aes(x = lon, y = lat), size = 1.5, alpha = 1/5, color = "blue")
#ggtitle("Donald Totals")

#multiplot(p1, p2, cols=2)

#ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HillaryTotals, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "red")
