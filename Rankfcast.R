# For a given time-srie (Song's ranks) this function will test three different models and choose the best model based on the lowes RMSE, models that will be tested are:
## Method 1: Average method
## Method 2: Random Walk(Naive forecast)
## Method 3: Random Walk(Naive forecast) with draft
library(readxl)
library(tidyverse)
library(ggfortify)
library(ggplot2)
library(tsbox)
library(imputeTS)
library(cowplot)
library(forecast)
library(dplyr)


Rankfcast <- function(SongName, ArtistName, show_ImputedValues =FALSE, study_e = FALSE, BestModel_show = FALSE){
  #Import data
  SpotifyData <- read_excel("Data/SpotifyData_full.xlsx")
  
  #Extract song's historical ranks
  song_Rank <- data.frame(SpotifyData %>% filter(song == SongName & Artist == ArtistName) %>% select(Date,Rank))
  
  # convert df to time series
  song_Rank <- ts_ts(song_Rank)

  # Imputation by Interpolation method is used to fill missing values.
  if (any(is.na(song_Rank)) == TRUE) {
    if (show_ImputedValues == TRUE) {plot(ggplot_na_imputations(song_Rank, na_interpolation(song_Rank)))}
    song_Rank <- na_interpolation(song_Rank)}

  #Data partitioning
  train <- subset(song_Rank, end = length(song_Rank)-1)
  test <- subset(song_Rank, start= length(song_Rank))

  fit.avg <- meanf(train, h=1)
  RMSE.avg =round(accuracy(fit.avg, test),2)[2,2]
  fit.rwk <- rwf(train, 1)
  RMSE.rwk = round(accuracy(fit.rwk, test),2)[2,2]
  fit.rwkd <- rwf(train, 1, drift = TRUE)
  RMSE.rwkd =round(accuracy(fit.rwkd, test),2)[2,2]

  df <- data.frame(model = c('avg', 'rwk', 'rwkd'), RMSE = c(RMSE.avg, RMSE.rwk, RMSE.rwkd))

  Best.Model <- filter(df, RMSE == min(RMSE)) %>% select(model)

  if (Best.Model == 'avg') {Best.Model <- meanf(train, h=1)}
  else if (Best.Model == 'rwk') {Best.Model <- rwf(train, 1)}
  else if (Best.Model == 'rwkd') {Best.Model <- rwf(train, 1, drift = TRUE)}

  # Residuals of the best model
  if (study_e == TRUE) {checkresiduals(Best.Model)}

  #cat('\nBest model for this time series is:', Best.Model$method, '\n')

  if (BestModel_show == TRUE){
    plot(autoplot(Best.Model) +
           autolayer(fitted(Best.Model), series=paste(Best.Model$method)) +
           autolayer(song_Rank, series="Data") +
           ggtitle(paste("Forecasts from", Best.Model$method)) + xlab("Weeks") +
           ylab("Rank") +
           guides(colour=guide_legend(title="Forecast")))
  }
  RMSE <- round(accuracy(Best.Model, test),2)[2,2]
  return(c(SongName, ArtistName, Best.Model$method,round(Best.Model$mean,0), test, RMSE))
}


#rank_pred <- Rankfcast(SongName = 'Sıcak Şarap', ArtistName = 'Batuhan Kordel', show_ImputedValues = F, study_e = F, BestModel_show = T)