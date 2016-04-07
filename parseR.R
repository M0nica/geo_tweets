#filename <- "loc_tweets.03.01.2016.json"
#date <- "03-01"

#generates csv files for each candidate for each date - 4 x 9 = 36 csv files

files <-c("loc_tweets.02.09.2016.json", "loc_tweets.02.20.2016.json", "loc_tweets.02.23.2016.json", "loc_tweets.02.27.2016.json", "loc_tweets.03.01.2016.json","loc_tweets.03.05.2016.json", "loc_tweets.03.06.2016.json", "loc_tweets.03.08.2016.json", "loc_tweets.03.15.2016.json") 
for (filename in files)
{
  tweets_shiny.df <- parseTweets(filename, simplify = TRUE)
  #make tweets go to lowercase 
  HillaryClinton <- tweets_shiny.df[grep("Hillary Clinton", tweets_shiny.df$text), ]
  HillaryClinton_wo <- tweets_shiny.df[grep("HillaryClinton", tweets_shiny.df$text), ]
  HillaryTotals <- rbind(HillaryClinton, HillaryClinton_wo)
  #write.csv(HillaryTotals, file = "HCTotals" + date + ".csv")
  write.csv(HillaryTotals, file = paste("HCTotals", filename, ".csv"))
  #load(HillaryTotals)
  
  #tweets_shiny.df <- parseTweets("all_candidates.json", simplify = TRUE)
  BernieSanders<- tweets_shiny.df[grep("Bernie Sanders", tweets_shiny.df$text), ]
  BernieSanders_wo <- tweets_shiny.df[grep("BernieSanders", tweets_shiny.df$text), ]
  BernieTotals <- rbind(BernieSanders, BernieSanders_wo)
  write.csv(BernieTotals, file = paste("BSTotals", filename, ".csv"))
  
  #tweets_shiny.df <- parseTweets("all_candidates.json", simplify = TRUE)
  DonaldTrump<- tweets_shiny.df[grep("Donald Trump", tweets_shiny.df$text), ]
  DonaldTrump_wo <- tweets_shiny.df[grep("DonaldTrump", tweets_shiny.df$text), ]
  DonaldTotals <- rbind(DonaldTrump, DonaldTrump_wo)
  write.csv(DonaldTotals, file = paste("DTTotals", filename, ".csv"))
  
  
  #tweets_shiny.df <- parseTweets("all_candidates.json", simplify = TRUE)
  #TedCruz<- tweets_shiny.df[grep("Ted Cruz", tweets_shiny.df$text), ]
  #TedCruz_wo <- tweets_shiny.df[grep("TedCruz", tweets_shiny.df$text), ]
  #TedTotals <- rbind(TedCruz, TedCruz_wo)
  
  
  #tweets_shiny.df <- parseTweets("all_candidates.json", simplify = TRUE)
  MarcoRubio<- tweets_shiny.df[grep("Marco Rubio", tweets_shiny.df$text), ]
  MarcoRubio_wo <- tweets_shiny.df[grep("MarcoRubio", tweets_shiny.df$text), ]
  MarcoTotals <- rbind(MarcoRubio, MarcoRubio_wo)
  write.csv(MarcoTotals, file = paste("MRTotals", filename, ".csv"))
  #load(MarcoTotals)
}
