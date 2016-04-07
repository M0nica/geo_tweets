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

#for wordcloud
#library(streamR)
#library(tm)
#library(wordcloud)
source("parseR.R")

#used static data as opposed to streaming!
#load("my_oauth.Rdata")
#load("all_candidates.json")

#tweets_shiny.df <- parseTweets("all_candidates.json", simplify = TRUE)
#tweets_shiny.df <- parseTweets("loc_tweets.03.01.2016.json", simplify = TRUE)




#creates a wordcloud from input dataframe
# 
# toCorpus <- function(dframe) {
#   text <- sapply(dframe$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
#   #removes all http from text
#   text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)
#   TweetCorpus <- paste(unlist(text), collapse =" ") 
#   TweetCorpus <- Corpus(VectorSource(TweetCorpus))
#   TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
#   TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
#   TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
#   # TweetCorpus <- tm_map(TweetCorpus, stemDocument)
#   TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
#   TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
#   #want to remove candidates from their own wordcloud
#   TweetCorpus <- tm_map(TweetCorpus, removeWords, c("like","will", "president", "say", "realjameswood", "for", "think", "the", "dont", "new", "amp", "get", "now","via", "this", "presid", "end", "while", "doesnt", "httpstcoau9loazlxp",  "httpstcoatbyuvqbkp", "https", "chrischristi", "that", "httpstco", "httpst", "http", "car", "chris", "christi", "hillaryclinton","johnkasich", "marcorubio", "hillari", "carlyfiorina", "fiorina", "hillary",  "berniesand", "sander", "berni", "realdonaldtrump", "bernie", "sanders", "donald", "trump", "clinton", 
#                                                     "hillary clinton", "bernie sanders", "jeb bush", "marco rubio", "ted cruz", "donald trump", "john kasich", "ben carson", "chris christie", "carly fiorina", "hillaryclinton", 
#                                                     "jebbush", "berniesanders", "marcorubio", "chrischristie", "bencarson", "johnkasich", "donaldtrump", "tedcruz", "carlyfiorina"
#   ))
#   return(TweetCorpus)
# }

toGeo <- function(dframe) {
#geo_tweets = parseTweets("loc_tweets.03.01.2016.json")

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
#geo_tweets = parseTweets("loc_tweets.02.09.2016.json")


ggplot()+ geom_polygon( data=US_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )

ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo_tweets, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "blue")


return(US_states)
}


#creates wordcloud for each candidate
#  BernieCorpus <- toCorpus(BernieTotals)
#  HillaryCorpus <- toCorpus(HillaryTotals)
#  TedCorpus <- toCorpus(TedTotals)
#  MarcoCorpus <- toCorpus(MarcoTotals)
#  DonaldCorpus <- toCorpus(DonaldTotals)
BernieCorpus <- toGeo(BernieTotals)
HillaryCorpus <- toGeo(HillaryTotals)
TedCorpus <- toGeo(TedTotals)
MarcoCorpus <- toGeo(MarcoTotals)
DonaldCorpus <- toGeo(DonaldTotals)


#logic behind the UI of the R application

shinyServer(function(input, output) {
  
  output$wordcloudT <- renderPlot({
    if (input$var == "Hillary Clinton"){
     # corpus <- HillaryCorpus
      geo_tweets <- HillaryCorpus
    } else if (input$var == 'Bernie Sanders'){
      #corpus <- BernieCorpus
      geo_tweets <- BernieCorpus
    } else if (input$var == 'Ted Cruz'){
      #corpus <- TedCorpus
      geo_tweets <- TedCorpus
    }
    else if  (input$var == 'Donald Trump'){
      #corpus <-DonaldCorpus
      geo_tweets <- DonaldCorpus
    }
    else if  (input$var == 'Marco Rubio'){
      geo_tweets <- MarcoCorpus
     # corpus <-MarcoCorpus
    }
    # wordcloud(corpus, max.words = input$wordCount, random.order = FALSE,scale=c(3.75,.05), min.freq=2, colors=brewer.pal(8, "Blues"))
  
    
    #ggplot()+ geom_polygon( data=US_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
    
    ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo_tweets, aes(x = lon, y = lat), size = 1, alpha = 1/5, color = "blue")
    
    
    return(US_states)
    })
  
  
#   output$freq_plot<- renderPlot({
#     
#     
#     if (input$var == "Hillary Clinton"){
#       filename <- HillaryTotals
#     } else if (input$var == 'Bernie Sanders'){
#       filename <- BernieTotals
#     } else if (input$var == 'Ted Cruz'){
#       filename <- TedTotals
#     }
#     else if  (input$var == 'Donald Trump'){
#       filename <-DonaldTotals
#     }
#     else if  (input$var == 'Marco Rubio'){
#       filename <-MarcoTotals
#     }
#   })
    
#     
#     #displays histogram associated with input filename
#     times <- as.POSIXct(filename$created_at, format="%a %b %d %H:%M:%S %z %Y")
#     hist(times, breaks=35)
#     
#   })
#   
#   
#   
#   #shows users where their cursor is currently
#   output$info_click <- renderText({
#     paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
#   })
  
  
  #lets users know which candidate they have selected and who's data they are looking at
  output$text1 <- renderText({ 
    paste("You have selected ", input$var)
  })
  
  
}
)