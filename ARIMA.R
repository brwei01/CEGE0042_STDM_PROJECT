library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)

# This file perform ARIMA on locationally-averaged counts in a time series.
# =======================================================
# load data
# =======================================================
current_directory <- getwd()
setwd(current_directory)
df <- read.csv('Data/C_Utah_Traffic.csv')


meanTraf <- colMeans(df[,5:(ncol(df))])
meanTraf_mat <- t(as.matrix(meanTraf))

plot(meanTraf, 
     ylab="hourly overall traffic flow of study area", 
     xlab="Time (in hours)", 
     type='l')

# lag plots
lag.plot(meanTraf, lags=6, do.lines = FALSE)

# the dependence between consecutive observations is linear.
# points separated by two or three lags are successively less dependent.
# the slopes, and therefore the correlations, are all positive



# Autocorrelation analysis
acf(meanTraf, 
    lag.max=48, 
    xlab="Lag", 
    ylab="ACF",
    main="Autocorrelation plot of hourly traffic flow")


# differenced auto correlation
EA.s.diff <- diff(meanTraf, lag=24, differences=1)
acf(EA.s.diff, 
    lag.max=48, 
    xlab="Lag", 
    ylab="ACF", 
    main="Differenced (1) autocorrelation plot")


# PACF
pacf(meanTraf, 
     lag.max=48,
     xlab="Lag",
     ylab="PACF",
     main="PACF, all stations averaged")

pacf(EA.s.diff, 
     lag.max=48, 
     xlab="Lag", 
     ylab="ACF",
     main="PACF, all stations averaged, 1st diff on 24 hrs lag")


# ==========================================================================================
# Parameter estimation and fitting
# this is ARIMA on averaged traffic flow of all stations
# ==========================================================================================
n_train <- ncol(meanTraf) * 0.7
fit.ar <- arima(meanTraf,order=c(3,0,15),seasonal=list(order=c(2,1,2),period=24))
fit.ar 

tsdiag(fit.ar)

pre.ar<-predict(fit.ar, n.ahead=48)
matplot(521:568,cbind(meanTraf[521:568],pre.ar$pred),type="l",main="", xlab="Hour", ylab="Average traffic flow")


# ==========================================================================================
# parameter tuning
# ==========================================================================================
library('tidyverse')
library('tseries')
library('forecast')

fit.Ar <- Arima(traf_matrix["354",1:n_train],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=24))
pre.Ar <- Arima(traf_matrix["354",n_train:(ncol(traf_matrix))],model=fit.Ar)
matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l")







