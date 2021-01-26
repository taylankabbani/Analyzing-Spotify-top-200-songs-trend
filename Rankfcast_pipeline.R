# For a given time-srie (Song's ranks) this function will test three different models and choose the best model based on the lowes RMSE, models that will be tested are:
## Method 1: Average method
## Method 2: Random Walk(Naive forecast)
## Method 3: Random Walk(Naive forecast) with draft
## Method 4: Holt's method with optimized alpha
## Method 5: Holt's method with optimized alpha and damped trend
library(readxl)
library(tidyverse)
library(ggfortify)
library(ggplot2)
library(tsbox)
library(imputeTS)
library(cowplot)
library(forecast)
library(dplyr)
#Import data
SpotifyData <- read_excel("Data/SpotifyData_full.xlsx")

Rankfcast <- function(SongName, ArtistName, show_ImputedValues =FALSE, study_e = FALSE, BestModel_show = FALSE){
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
  
  #manually identify the optimal holt alpha 
  Optimal_param <- function(data){
    train <- subset(data, end = length(data)-1)
    validate <- subset(data, start= length(data))
    # loop through a series of betas and alphas
    param <- seq(.0001, 1, by = .001)
    RMSE_alpha <- NA
    for(i in seq_along(param)) {
      fit_alpha <- holt(train, alpha = param[i], h = 1)
      RMSE_alpha[i] <- accuracy(fit_alpha, validate)[2,2]
    }
    
    # convert to a data frame and idenitify min alpha and beta value
    df_1 <- data_frame(param, RMSE_alpha)
    alpha.opt <- filter(df_1, RMSE_alpha == min(RMSE_alpha)) %>% select(param,RMSE_alpha)
    # plot RMSE vs. alpha
    return(alpha.opt)}
  
  param <- Optimal_param(song_Rank)
  Alpha <- as.numeric(param[[1]][1])
  
  #Fitting holt model
  fit.holt <- holt(train, 1, alpha = Alpha, damped = F)
  RMSE.holt = round(accuracy(fit.holt, test),2)[2,2]
  #Fitting holt model with damped trend
  fit.holtd <- holt(train, 1, alpha = Alpha, damped = T)
  RMSE.holtd = round(accuracy(fit.holtd, test),2)[2,2]
  
  # Forecast future Rank
  fcast.model <- NA
  
  df <- data.frame(model = c('avg', 'rwk', 'rwkd', 'holt','holtd'), 
                   RMSE = c(RMSE.avg, RMSE.rwk, RMSE.rwkd,RMSE.holt, RMSE.holtd))

  Best.Model <- filter(df, RMSE == min(RMSE)) %>% select(model)

  if (Best.Model == 'avg') {Best.Model <- meanf(train, h=1) 
                            fcast.Model <- meanf(song_Rank, h=1)}
  
  else if (Best.Model == 'rwk') {Best.Model <- rwf(train, 1) 
                                  fcast.Model <- rwf(song_Rank, 1)}
  
  else if (Best.Model == 'rwkd') {Best.Model <- rwf(train, 1, drift = TRUE) 
                                  fcast.Model <- rwf(song_Rank, 1, drift = TRUE)}
  
  else if (Best.Model == 'holt') {Best.Model <- holt(train, 1, alpha = Alpha, damped = F)
                                  fcast.Model <- holt(song_Rank, 1, alpha = Alpha, damped = F)}
  
  else if (Best.Model == 'holtd'){Best.Model <- holt(train, 1, alpha = Alpha, damped = T)
                                  fcast.Model <- holt(song_Rank, 1, alpha = Alpha, damped = T)}

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
  
  predicted <- round(fcast.Model$mean,0)
  return(c(SongName, ArtistName, Best.Model$method,round(Best.Model$mean,0), test, RMSE, predicted))
}


# rank_pred <- Rankfcast(SongName = 'Rauf', ArtistName = 'Rauf & Faik',
# show_ImputedValues = F, study_e = F, BestModel_show = F)