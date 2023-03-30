# https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html#distance-band-weights
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
# STARIMA
# ===============================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
source("Packages/starima_package.R")
df <- read.table('Data/C_Utah_Traffic.csv', header = TRUE, sep = ",")

# ===============================================================
# building weight matrix by KNN
stations <- data.frame(cbind(df$long, df$lat))
colnames(stations) <- c('easting','northing')
coords <- SpatialPoints(stations)

duplicates <- duplicated(stations$easting)
stations[duplicates,]

proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
coords <- spTransform(coords, CRS("+proj=utm +zone=12 +datum=WGS84"))
coords <- data.frame(coords@coords)
coords <- cbind(coords$northing, coords$easting)

duplicates <- duplicated(coords)
coords[duplicates,]


coords <- as.matrix(coords)
class(coords)

# assuming that every node connects to 4 roads
k <- knn2nb(knearneigh(coords, k=4))
# convert nb to matrix
k_mat <- nb2mat(k)
write.csv(k_mat, file='Data/W_Utah_Traffic.csv')

'''
k.card <- card(k)
ggplot() +
  geom_histogram(aes(x=k.card), binwidth =  .01) +
  xlab("Number of Neighbors")
plot(k, coords, lwd=.2, col="blue", cex = .5)
'''

# ===============================================================
# load weight matrix
W <- read.table(file='Data/W_Utah_Traffic.csv',header=TRUE, sep=',')
W$X = NULL
W <- as.matrix(W)

# convert data into matrix form
traf.mat <- t(as.matrix(df[,5:ncol(df)]))
stacf(traf.mat, W, 48)

# first difference
traf.mat.diff1 <- diff(traf.mat, lag=24, differences=1)
stpacf(traf.mat.diff1, W, 48)

# parameter esitimation
n_train = round(0.7*nrow(traf.mat))
print(paste0('=== The length of training data is ', n_train, " ==="))

W_fit<-list(w1=W)
fit.star <- starima_fit(traf.mat[1:n_train,],W_fit,p=3,d=24,q=2)

# Diagnostic Checking
stacf(fit.star$RES,W,48)
hist(fit.star$RES[,10])

# prediction with the STARIMA model
pre.star <- starima_pre(traf.mat[(n_train-24-5+1):nrow(traf.mat),], model=fit.star)
df1 = data.frame(t(df[,5:ncol(df)]))
matplot(1:223, cbind(df1[(n_train+1):nrow(traf.mat),1], pre.star$PRE[,1]),type='l')



              