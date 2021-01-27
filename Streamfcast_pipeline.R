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
RankPrediction <- read_excel("Data/Top200Prediction.xlsx")
top100 <- read_excel('Data/top100.xlsx') %>% select('song','artist')


Streamfcast <- function(SongName, ArtistName, show_ImputedValues =FALSE, study_e = FALSE, BestModel_show = FALSE){
  #Extract song's historical ranks
  song_Stream <- data.frame(SpotifyData %>% filter(song == SongName & Artist == ArtistName) %>% select(Date,Streams))
  song_Rank <- data.frame(SpotifyData %>% filter(song == SongName & Artist == ArtistName) %>% select(Date,Rank))

  # The predicted Rank in train for the song
  f_rank_train <- as.numeric(RankPrediction %>% filter(song == SongName & Artist == ArtistName) %>% select(train_pred))
  # convert df to time series
  song_Stream <- ts_ts(song_Stream)
  song_Rank <- ts_ts(song_Rank)

  # Imputation by Interpolation method is used to fill missing values.
  if (any(is.na(song_Stream)) == TRUE) {
    if (show_ImputedValues == TRUE) {plot(ggplot_na_imputations(song_Stream, na_interpolation(song_Stream)))}
    song_Stream <- na_interpolation(song_Stream)
    song_Rank <- na_interpolation(song_Rank)}


  #Data partitioning
  train <- subset(song_Stream, end = length(song_Stream)-1)
  test <- subset(song_Stream, start= length(song_Stream))

  train_xreg <- subset(song_Rank, end = length(song_Rank)-1)
  test_xreg  <- subset(song_Rank, start= length(song_Rank))

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

  param <- Optimal_param(song_Stream)
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

  # ETS model
  fit.ets <- ets(train, model='ZZZ')
  RMSE.ets <- round(accuracy(forecast(fit.ets, h=1), test),2)[2,2]

  # arima model
  fit.arima <- auto.arima(train, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
  RMSE.arima <- round(accuracy(forecast(fit.arima, h=1), test),2)[2,2]

  #Exp regression
  fit.exp <- tslm(train ~ trend, lambda = 0)
  RMSE.exp <- round(accuracy(forecast(fit.exp, h=1), test),2)[2,2]

  #Dynamic Regression method
  fit.xreg <- auto.arima(train, xreg = train_xreg)
  RMSE.xreg <- round(accuracy(forecast(fit.xreg,xreg =f_rank_train,h=1), test),2)[2,2]




  df <- data.frame(model = c('holtA', 'holtAd','holtB','holtBd' ,'ETS', 'ARIMA','ExpReg','Xreg'),
                   RMSE = c(RMSE.holtA, RMSE.holtAd, RMSE.holtB,RMSE.holtBd,RMSE.ets,RMSE.arima, RMSE.exp, RMSE.xreg))

  Best.Model <- filter(df, RMSE == min(RMSE)) %>% select(model)

  if (Best.Model == 'holtA') {Best.Model <- holt(train, h=1,alpha = Alpha,damped = F,initial = 'optimal')
                              RMSE <- round(accuracy(Best.Model, test),2)[2,2]
                              fcast.Model <- holt(song_Stream, h=1,alpha = Alpha,damped = F,initial = 'optimal')}

  else if (Best.Model == 'holtAd') {Best.Model <- holt(train, h=1, alpha = Alpha, damped = T,initial = 'optimal')
                                   RMSE <- round(accuracy(Best.Model, test),2)[2,2]
                                   fcast.Model <-  holt(song_Stream, h=1, alpha = Alpha, damped = T,initial = 'optimal')}

  else if (Best.Model == 'holtB') {Best.Model <- holt(train, h=1, beta = Beta, damped = F,initial = 'optimal')
                                  RMSE <- round(accuracy(Best.Model, test),2)[2,2]
                                  fcast.Model <- holt(song_Stream, h=1, beta = Beta, damped = F,initial = 'optimal')}

  else if (Best.Model == 'holtBd') {Best.Model <- holt(train, h=1, beta = Beta, damped = T,initial = 'optimal')
                                   RMSE <- round(accuracy(Best.Model, test),2)[2,2]
                                   fcast.Model <- holt(song_Stream, h=1, beta = Beta, damped = T,initial = 'optimal')}

  else if (Best.Model == 'ETS') {Best.Model <- ets(train, model='ZZZ')
                                RMSE <- round(accuracy(forecast(Best.Model,h=1), test),2)[2,2]
                                fcast <- ets(song_Stream, model='ZZZ')
                                fcast.Model <- forecast(fcast,h=1)}

  else if (Best.Model == 'ARIMA') {Best.Model <- auto.arima(train, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
                                  RMSE <- round(accuracy(forecast(Best.Model,h=1), test),2)[2,2]
                                  fcast <- auto.arima(song_Stream, seasonal = FALSE, approximation = FALSE, stepwise = FALSE)
                                  fcast.Model <- forecast(fcast,h=1)}

  else if (Best.Model == 'ExpReg'){Best.Model <- tslm(train ~ trend, lambda = 0)
                                    RMSE <- round(accuracy(forecast(Best.Model,h=1), test),2)[2,2]
                                    fcast <- tslm(song_Stream ~ trend, lambda = 0)
                                    fcast.Model <- forecast(fcast,h=1)}

  else if (Best.Model == 'Xreg'){Best.Model <- auto.arima(train, xreg = train_xreg)
                                RMSE <- round(accuracy(forecast(Best.Model,xreg=f_rank_train ,h=1), test),2)[2,2]
                                fcast<- auto.arima(song_Stream, xreg = song_Rank)
                                fcast.Model <- forecast(fcast,xreg= f_rank_train,h=1)}

  # Residuals of the best model
  if (study_e == TRUE) {checkresiduals(Best.Model)}

  if (BestModel_show == TRUE){
    plot(autoplot(Best.Model) +
           autolayer(fitted(Best.Model), series=paste(Best.Model$method)) +
           autolayer(song_Stream, series="Data") +
           ggtitle(paste("Forecasts from", Best.Model$method)) + xlab("Weeks") +
           ylab("Rank") +
           guides(colour=guide_legend(title="Forecast")))
  }
  predicted <- fcast.Model$mean

  return(c(SongName, ArtistName,RMSE, predicted,Best.Model$method))

}

# stream_pred <- Streamfcast(SongName = "Bil Diye Söylüyorum" , ArtistName =  "Sibel Can",
#                          show_ImputedValues = F, study_e = F, BestModel_show = F)
# stream_pred


s <- list()
# Iterate over each song using the Rankfcast pipeline
for (i in 1:dim(top100)[1]) {
  song <- as.character(top100[i,1])
  artist <- as.character(top100[i,2])
  print(i)
  s[[i]] <- Streamfcast(song, artist)
  print(s[[i]])
}

# Convert to dataframe
Streams_fcasts <- as.data.frame(do.call(rbind,s))
names(Streams_fcasts) = c('song','Artist','RMSE','Model','Fcast')
# write.xlsx2(Streams_fcasts,'Streamfcast.xlsx',sheetName = "Sheet1",col.names = TRUE, row.names=FALSE)
