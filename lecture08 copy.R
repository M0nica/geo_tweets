setwd("~/Documents/geo_tweets")
library(streamR)
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)
library(ggplot2)

geo_tweets = parseTweets("loc_tweets.03.01.2016.json")
