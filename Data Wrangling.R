# This file will divide the songs into three categories based on the number of data points available to retrieve in the past:
# 1. Very short time-series: <= 10 weeks
# 2. Short time-series: > 10 weeks but <= 54 weeks (less than a year)
# 3. long time-series: > 54 weeks (more than one year)
# Note: for some Russian characters the song name replaced with the singer's name
# Taking the name of the song and the singer because there are some songs with the sane name(Dunyadan uzak)
library(readxl)
library(tidyverse)
library(xlsx)

SpotifyData <- read_excel("Data/SpotifyData_full.xlsx")

# list of songs (along with artists) names that will be forecasted (200 top songs for week 2021-01-07)
songs_lis <- SpotifyData %>% filter(Date == as.Date('2021-01-07')) %>% select(song)  %>% .$song
artists_lis <- SpotifyData %>% filter(Date == as.Date('2021-01-07')) %>% select(Artist)  %>% .$Artist



# Extracting specific song & Artists streams
FindSong <- function(SongName, ArtistName){
  song_streams <- as.data.frame(SpotifyData %>% filter(song == SongName & Artist == ArtistName)
                                %>% select(Date,Streams))
  song_streams}

# Counting how many weeks in the past does exist for each song in our data
retrived_counts <- NA
for (i in 1:200) {
  counts <- FindSong(songs_lis[i], artists_lis[i]) %>% count()
  retrived_counts[i] <- as.numeric(counts)
}
counts_df <- cbind('song'= data.frame('song'=songs_lis),data.frame('artist'= artists_lis), 
                   data.frame('counts'=retrived_counts))

# Saving songs names for each category

# V_short <- counts_df %>% filter(counts <= 10) %>% select(song, artist) %>% 
#   write.xlsx2('Vshort_ts.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
#  
# short <- counts_df %>% filter(counts > 10 & counts <= 54) %>% select(song, artist) %>% 
#     write.xlsx2('short_ts.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
# 
# long <- counts_df %>% filter(counts > 54) %>% select(song,artist) %>% 
#   write.xlsx2('long_ts.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
# top100 <- counts_df[1:100,1:3] %>% write.xlsx2('top100.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
