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
library(xlsx)
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
  fit.rwk <- rwf(train, h=1)
  RMSE.rwk = round(accuracy(fit.rwk, test),2)[2,2]
  fit.rwkd <- rwf(train, h=1, drift = TRUE)
  RMSE.rwkd =round(accuracy(fit.rwkd, test),2)[2,2]
  
  #manually identify the optimal holt alpha and beta
  Optimal_param <- function(data, show=FALSE){
    train <- subset(data, end = length(data)-1)
    validate <- subset(data, start= length(data))
    # loop through a series of betas and alphas
    param <- seq(.0001, 1, by = .001)
    RMSE_beta <- NA
    RMSE_alpha <- NA
    
    for(i in seq_along(param)) {
      fit_alpha <- holt(train, alpha = param[i], h = 1)
      RMSE_alpha[i] <- accuracy(fit_alpha, validate)[2,2]
      fit_beta <- holt(train, beta = param[i], h = 1)
      RMSE_beta[i] <- accuracy(fit_beta, validate)[2,2]
    }
    
    # convert to a data frame and idenitify min alpha and beta value
    df <- data_frame(param, RMSE_alpha, RMSE_beta)
    alpha.opt <- filter(df, RMSE_alpha == min(RMSE_alpha)) %>% select(param,RMSE_alpha)
    beta.opt <- filter(df, RMSE_beta == min(RMSE_beta)) %>% select(param,RMSE_beta)
    
    # plot RMSE vs. alpha
    if (show==TRUE) {
      plot <- ggplot(df, aes(param,RMSE_alpha)) +
        geom_line(color='blue')+
        geom_line(aes(param,RMSE_beta),color = 'red')+
        geom_point(data = alpha.opt, aes(param, RMSE_alpha), size = 3, color = "blue")+
        geom_point(data = beta.opt, aes(param, RMSE_beta), size = 3, color = "Red")
      
      
      return(list(plot,alpha.opt,beta.opt))} else{return(list(alpha.opt,beta.opt))}
    
    
    
  }
  
  param <- Optimal_param(song_Rank)
  Alpha <- as.numeric(param[[1]][1])
  Beta<- as.numeric(param[[2]][1])
  ####################################Models#############
  #Fitting holt model with optimal Alpha
  fit.holtA <- holt(train, h=1, alpha = Alpha, damped = F,initial = 'optimal')
  RMSE.holtA = round(accuracy(fit.holtA, test),2)[2,2]
  
  #Fitting holt model with optimal Alpha and damped trend
  fit.holtAd <- holt(train, h=1, alpha = Alpha, damped = T,initial = 'optimal')
  RMSE.holtAd = round(accuracy(fit.holtAd, test),2)[2,2]
  
  #Fitting holt model with optimal Beta
  fit.holtB <- holt(train, h=1, beta = Beta, damped = F,initial = 'optimal')
  RMSE.holtB = round(accuracy(fit.holtB, test),2)[2,2]
  
  #Fitting holt model with optimal Alpha and damped trend
  fit.holtBd <- holt(train, h=1, beta = Beta, damped = T,initial = 'optimal')
  RMSE.holtBd = round(accuracy(fit.holtBd, test),2)[2,2]
  
  
  # Forecast future Rank
  fcast.model <- NA
  
  df <- data.frame(model = c('avg', 'rwk', 'rwkd', 'holtA','holtAd','holtB', 'holtBd'), 
                   RMSE = c(RMSE.avg, RMSE.rwk, RMSE.rwkd, RMSE.holtA, RMSE.holtAd, RMSE.holtB, RMSE.holtBd))

  Best.Model <- filter(df, RMSE == min(RMSE)) %>% select(model)

  if (Best.Model == 'avg') {Best.Model <- meanf(train, h=1) 
                            fcast.Model <- meanf(song_Rank, h=1)}
  
  else if (Best.Model == 'rwk') {Best.Model <- rwf(train, h=1) 
                                  fcast.Model <- rwf(song_Rank, h=1)}
  
  else if (Best.Model == 'rwkd') {Best.Model <- rwf(train, h=1, drift = TRUE) 
                                  fcast.Model <- rwf(song_Rank, h=1, drift = TRUE)}
  
  else if (Best.Model == 'holtA') {Best.Model <- holt(train, h=1, alpha = Alpha, damped = F,initial = 'optimal')
                                  fcast.Model <- holt(song_Rank, h=1, alpha = Alpha, damped = F,initial = 'optimal')}
  
  else if (Best.Model == 'holtAd'){Best.Model <- holt(train, h=1, alpha = Alpha, damped = T,initial = 'optimal')
                                  fcast.Model <- holt(song_Rank, h=1, alpha = Alpha, damped = T,initial = 'optimal')}
  
  else if (Best.Model == 'holtB'){Best.Model <- holt(train, h=1, beta = Beta, damped = F,initial = 'optimal')
                                  fcast.Model <- holt(song_Rank, h=1, beta = Beta, damped = F,initial = 'optimal')}
  else if (Best.Model == 'holtBd'){Best.Model <- holt(train, h=1, beta = Beta, damped = T,initial = 'optimal')
                                  fcast.Model <- holt(song_Rank, h=1, beta = Beta, damped = T,initial = 'optimal')}

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