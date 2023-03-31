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
library(tmap)


setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
df <- read.csv('Data/C_Utah_Traffic.csv')
stations <- tm_shape(stations)
traf_matrix <- data.matrix(df[,5:ncol(df)])

# ==============================================================
# TEMPORAL AUTO CORRELATION
# ==============================================================
# examine the lagged variables
meanTraf <- colMeans(df[,5:(ncol(df))])

hours <- seq(from = as.POSIXct("2022-01-01 00:00"),
                   to = as.POSIXct("2022-01-31 22:00"), 
                   by = "hour")
# hours_label <- as.character(hours)
# hours_label <- substr(hours_label, start=6, stop=13)


TrafLagged <- data.frame(hours, t=meanTraf[2:(length(meanTraf))], t_minus_1=meanTraf[1:(length(meanTraf)-1)])
p1 <- ggplot(TrafLagged, aes(x=hours, y=t)) + geom_line()
p2 <- ggplot(TrafLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  ggplot2::annotate("text", 8.5, 10, label=paste("r =", round(cor(TrafLagged$t, TrafLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)


# ==============================================================
# calculate over higher lags -- plot an ACF
# ==============================================================
acf(meanTraf, lag.max = 48) 
# the smaller the lag, the greater the correlation
# however negative correlation during night-time

acf(traf_matrix[1,], lag.max=48, main="ACF, station no.354")
# An ACF plot is generated automatically when the function is run. 95% confidence intervals are placed on the plot,

# plot seasonal (daily) patterns
acf(colMeans(matrix(traf_matrix[1,],24)), main="ACF, station no.354 daily average")
# obvious week pattern shown on graph --- cyclic 7 bars


# ==============================================================
# Partial Correlation Functions: plot a PACF
# ==============================================================

pacf(traf_matrix[1,], lag.max=48, main="PACF, station no.354")




# ==============================================================
# SPATIAL AUTO CORRELATION
# ==============================================================
# load weight matrix
W <- read.table(file='Data/WD_Utah_Traffic.csv',header=TRUE, sep=',')
W$X = NULL
W <- as.matrix(W)
W <- mat2listw(W)

# ==============================================================
# global moran -- averaging temperature over locations
traf_avg <- rowMeans(traf_matrix)
moran.test(x=traf_avg, listw=W)

# monte-carlo simulation of moran I
moran.mc(traf_avg, listw=W, nsim=9999)
# 90% confidence level to assume the traffic as spatially correlated


# ==============================================================
# Local moran
lm <- localmoran(x=rowMeans(traf_matrix), listw=W)
lm

# ==============================================================
# point data

stations <- data.frame(cbind(df$long, df$lat))
colnames(stations) <- c('easting','northing')
coords <- SpatialPoints(stations)
points <- coords

proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
coords <- spTransform(coords, CRS("+proj=utm +zone=12 +datum=WGS84"))
coords <- data.frame(coords@coords)
coords <- cbind(coords$easting, coords$northing)
class(coords)

coords_lst = list(coords)
class(coords_lst)

plot(variogram(list(rowMeans(traf_matrix)), locations=coords_lst))
# the x-axis is the separation distance in metres and the y-axis is the semivariance. As expected, the semivariance increases as the separation distance increases.

# examine in different directions
plot(variogram(list(rowMeans(traf_matrix)), locations=coords_lst, alpha=c(0,45,90,135)))




# ==============================================================
# SPATIAL TEMPORAL AUTO CORRELATION
# ==============================================================
source("Packages/starima_package.R")
W <- listw2mat(W)
stacf(t(traf_matrix), W, 48)
stpacf(t(uk_temp_matrix), Wmat, 4)




# ==============================================================
# space-time semivariogram
# Project the points to get spatial lags in metres
pts <- SpatialPoints(coords, 
                     proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# hours to date format
hours <- seq(from = as.POSIXct("2022-01-01 00:00"),
             to = as.POSIXct("2022-01-31 23:00"), 
             by = "hour")
stfdf <- STFDF(pts, hours, data.frame(as.vector(t(traf_matrix))))
names(stfdf@data) <- "Traffic"


# Calculate ST-semivariogram with bin width 100km, max lag 1000km.
TrSTVar <- variogram(Traffic~1, stfdf, width=1, cutoff=10,tlags=0:24)


