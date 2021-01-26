library(readxl)
vshort_songs <- read_excel("Data/Vshort_ts.xlsx")
short_songs <- read_excel("Data/short_ts.xlsx")
long_songs <- read_excel("Data/long_ts.xlsx")

# Note: there are 18 songs that could not be handled by the pipeline because of lacking regular pattern
songs <- rbind(vshort_songs,short_songs,long_songs)

Rank_fcasts <- list()

# Iterate over each song using the Rankfcast pipeline
for (i in 180:dim(songs)[1]) {
  song <- as.character(songs[i,1])
  artist <- as.character(songs[i,2])
  Rank_fcasts[[i]] <- Rankfcast(song,artist)
}

# Convert to dataframe
Rank_fcasts <- as.data.frame(do.call(rbind,Rank_fcasts))
names(Rank_fcasts) = c('song','Artist','Model','train_pred','test', 'RMSE','Fcast')