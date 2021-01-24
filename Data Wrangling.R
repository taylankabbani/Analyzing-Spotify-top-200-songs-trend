# This file will divide the songs into three categories based on the number of data points available to retrieve in the past:
# 1. Very short time-series: <= 10 weeks
# 2. Short time-series: > 10 weeks but <= 54 weeks (less than a year)
# 3. long time-series: > 54 weeks (more than one year)

library(readxl)
library(tidyverse)

SpotifyData <- read_excel("Data/SpotifyData.xlsx")


# list of songs (along with artists) names that will be forecasted (200 top songs for week 2021-01-07)
songs_lis <- SpotifyData %>% filter(Date == as.Date('2021-01-07')) %>% select(song)  %>% .$song
artists_lis <- SpotifyData %>% filter(Date == as.Date('2021-01-07')) %>% select(Artist)  %>% .$Artist


# Extracting specific song & Artists streams
FindSong <- function(SongName, Artist){
  song_streams <- as.data.frame(SpotifyData %>% filter(song == SongName & Artist == Artist)
                                %>% select(Date,Streams))
  song_streams}

# Counting how many weeks in the past does exist for each song in our data
retrived_counts <- NA
for (i in 1:200) {
  counts <- FindSong(songs_lis[i], artists_lis[i]) %>% count()
  retrived_counts[i] <- as.numeric(counts)
}
counts_df <- cbind(data.frame(songs_lis), data.frame(retrived_counts))

# Saving songs names for each category

V_short <- counts_df %>% filter(retrived_counts <= 10) %>% select(songs_lis) %>% 
  .$songs_lis %>% write.csv("Vshort_ts.csv", row.names = F, quote = F)

short <- counts_df %>% filter(retrived_counts > 10 & retrived_counts <= 54) %>% 
  write.csv("short_ts.csv",     row.names = F, quote = F)

long <- counts_df %>% filter(retrived_counts > 54) %>% write.csv("long_ts.csv", row.names = F, quote = F)