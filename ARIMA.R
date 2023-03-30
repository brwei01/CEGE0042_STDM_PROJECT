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

# =======================================================
# load data
# =======================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
df <- read.csv('Data/C_Utah_Traffic.csv')

traf_matrix <- data.matrix(df[,5:ncol(df)])
rownames(traf_matrix) <- df$X
plot(traf_matrix["615",], 
     ylab="hourly traffic flow", 
     xlab="Time (in hours)", 
     type='l')


# lag plots
lag.plot(traf_matrix['615',], lags=3, do.lines = FALSE)

# the dependence between consecutive observations is linear.
# points separated by two or three lags are successively less dependent.
# the slopes, and therefore the correlations, are all positive



# Autocorrelation analysis
acf(traf_matrix["615",], 
    lag.max=48, 
    xlab="Lag", 
    ylab="ACF",
    main="Autocorrelation plot of hourly traffic flow")


# differenced auto correlation
EA.s.diff <- diff(traf_matrix["615",], lag=24, differences=1)
acf(EA.s.diff, 
    lag.max=48, 
    xlab="Lag", 
    ylab="ACF", 
    main="Differenced (1) autocorrelation plot")


# PACF
pacf(traf_matrix["615",], 
     lag.max=48,
     xlab="Lag",
     ylab="PACF",
     main="Partial Autocorrelation plot of hourly traffic flow")

pacf(EA.s.diff, 
     lag.max=48, 
     xlab="Lag", 
     ylab="ACF",
     main="Partial Autocorrelation plot of hourly average traffic")



#Parameter estimation and fitting
n_train <- ncol(traf_matrix) * 0.7
fit.ar <- arima(traf_matrix["615",1:n_train],order=c(1,0,2),seasonal=list(order=c(1,1,0),period=24))
fit.ar 

tsdiag(fit.ar)

pre.ar<-predict(fit.ar, n.ahead=48)
matplot(521:568,cbind(traf_matrix["615",521:568],pre.ar$pred),type="l",main="", xlab="Hour", ylab="Average traffic flow")


# ==========================================================================================
# parameter tuning
# ==========================================================================================
library('tidyverse')
library('tseries')
library('forecast')

fit.Ar <- Arima(traf_matrix["615",1:n_train],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=24))
pre.Ar <- Arima(traf_matrix["615",n_train:(ncol(traf_matrix))],model=fit.Ar)
matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l")







