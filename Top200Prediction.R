library(readxl)
library(xlsx)
vshort_songs <- read_excel("Data/Vshort_ts.xlsx")
short_songs <- read_excel("Data/short_ts.xlsx")
long_songs <- read_excel("Data/long_ts.xlsx")

# Note: there are 18 songs that could not be handled by the pipeline because of lacking regular pattern
songs <- rbind(vshort_songs,short_songs,long_songs)

Rank_fcasts <- list()

# Iterate over each song using the Rankfcast pipeline
for (i in 1:dim(songs)[1]) {
  song <- as.character(songs[i,1])
  artist <- as.character(songs[i,2])
  print(i)
  Rank_fcasts[[i]] <- Rankfcast(song,artist)
  print(Rank_fcasts[[i]])
}

# Convert to dataframe
Rank_fcasts <- as.data.frame(do.call(rbind,Rank_fcasts))
names(Rank_fcasts) = c('song','Artist','Model','train_pred','test', 'RMSE','Fcast')
# write.xlsx2(Rank_fcasts,'Top200Prediction.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
