#title: "DataScraping"
#author: "Taylan Kabbani"
#date: "January 17, 2021"
#output: pdf_document

  
# Required packages:
library(rvest)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(tibble)
library(xlsx)


# Creating the URL Sequence

# fixed for all pages..........................Variable based on the date
#'https://spotifycharts.com/regional/tr/weekly/' + 'start_date--end_date'

# Create full Url
makeURL <- function(){
  # Urls list
  urls <- c()
  
  fixed_part <- 'https://spotifycharts.com/regional/tr/weekly/'
  
  # Define the Range for Our dates
  timevalues <- seq(as.Date("2016-12-23"), as.Date("2021-01-15"), by = "week")
  
  for (i in 1:(length(timevalues)-1)) {
    date <- paste(timevalues[i], timevalues[i+1],sep = '--')
    url <- paste(fixed_part, date, sep = '')
    urls <- c(urls,url)}
  urls
}

urls <- makeURL()


# Scraping using rvest package
SpotifyScrape <- function(page){
  rank <- page %>% read_html() %>% html_nodes('.chart-table-position') %>% html_text() %>% as.data.frame()
  song <- page %>% read_html() %>% html_nodes('strong') %>% html_text() %>% as.data.frame()
  artist <- page %>% read_html() %>% html_nodes('.chart-table-track span') %>% html_text() %>% as.data.frame()
  streams <- page %>% read_html() %>% html_nodes('td.chart-table-streams') %>% html_text() %>% as.data.frame()
  dates <- page %>% read_html() %>% html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>% html_text() %>% as.data.frame()
  
  #Create tibble
  tibl <- cbind(rank, song, artist, streams, dates)
  names(tibl) <- c("Rank", "song", "Artist", "Streams", "Date")
  tibl <- as.tibble(tibl)
  # Fix coulmn types
  tibl <- tibl%>% mutate(Rank = as.integer(Rank), Artist = str_remove(Artist,'by '), Streams = as.numeric(str_remove_all(Streams,',')), Date = as.Date(Date, format="%m/%d/%Y"))
  return(tibl)
}

# Scraping data
spotifyData <- map_df(urls, SpotifyScrape)

# Save data as xlsx
write.xlsx2(spotifyData, 'C:\\Users\\HPi7\\Downloads\\SpotifyData.xlsx',sheetName = "Sheet1",col.names = TRUE)
