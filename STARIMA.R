library(tmap)
library(sf)
library(spdep)
library(ggplot2)
library(deldir)
library(sp)
library(purrr)
library(rgdal)
library(maptools)
library(knitr)

# ===============================================================
# Load data
# ===============================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
source("Packages/starima_package.R")
df <- read.table('Data/C_Utah_Traffic.csv', header = TRUE, sep = ",")

# load weight matrix
W <- read.table(file='Data/WD_Utah_Traffic.csv',header=TRUE, sep=',')
W$X = NULL
W <- as.matrix(W)


# ===============================================================
# fitting STARIMA
# ===============================================================

# convert data into matrix
traf.mat <- t(as.matrix(df[,5:ncol(df)]))
# get starima acf plot before differencing
stacf(traf.mat, W, 48)

# calculate first difference
traf.mat.diff1 <- diff(traf.mat, lag=24, differences=1)
# plot starima pacf after first differecing
stpacf(traf.mat.diff1, W, 48)


# train-test split by 70% & 30%
n_train = round(0.7*nrow(traf.mat))
print(paste0('=== The length of training data is ', n_train, " ==="))

# fitting algorithm (parameter estimation)
W_fit<-list(w1=W)
fit.star <- starima_fit(traf.mat[1:n_train,],W_fit,p=3,d=24,q=1)

# Diagnostic Checking
stacf(fit.star$RES,W,48)
hist(fit.star$RES[,10], breaks = 20)

# prediction with the STARIMA model
pre.star <- starima_pre(traf.mat[(n_train-24-4+1):nrow(traf.mat),], model=fit.star)
df1 = data.frame(t(df[,5:ncol(df)]))
matplot(1:223, cbind(df1[(n_train+1):nrow(traf.mat),1], pre.star$PRE[,1]),type='l')


# getting the normalized MSE for STARIMA
all_NRMSE <- pre.star$NRMSE
all_NRMSE
pre.star$NRMSE[1]

# average NRMSE in all stations
mean(all_NRMSE)

              