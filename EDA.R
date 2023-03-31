# =======================================================
# load data
# =======================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
df <- read.csv('Data/N_Utah_Traffic.csv')
df_meta <- read.csv('Data/Utah_Traffic_Meta.csv')
# setting the first col(time) as indexes
row.names(df) <- df$X
df$X <- NULL
names(df) <- substring(names(df),2)

# transpose df and make it into matrix
traf_matrix <- t(df)


# =======================================================
# combining data with meta data
# saving the data with locational information
# =======================================================
# adding lat and long to traf_matrix
station <- as.numeric(rownames(traf_matrix))
long <- df_meta$longitude[station == df_meta$station_id]
lat <- df_meta$latitude[station == df_meta$station_id]
route <- df_meta$route[station == df_meta$station_id]
traf_matrix <- cbind(long, lat, route, traf_matrix)
write.csv(traf_matrix, file='Data/C_Utah_Traffic.csv')


# =======================================================
# load the new data with locations affiliated 
# =======================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
df <- read.csv('Data/C_Utah_Traffic.csv')
df_meta <- read.csv('Data/N_Utah_Traffic_Meta.csv')
colnames(df)[5:ncol(df)] <- substring(colnames(df)[5:ncol(df)], 2)
traf_matrix <- as.matrix(df[, 5:ncol(df)])


# =======================================================
# non-spatial EDA
# =======================================================
# getting mean and std of data
mu = mean(traf_matrix)
print(paste0('the mean traffic amount across all stations is', mu))
sdev = sd(traf_matrix)
print(paste0('the std of traffic amount across all stations is', sdev))

# plotting distribution
hist(traf_matrix)
abline(v=mu, col='red')
# data are not normally distributed and highly positively skewed

# examining the distribution i QQ plot
library(ggplot2)
qqnorm(traf_matrix)
qqline(traf_matrix, col='red')
# explanations to this might be: a lot of stations have low traffic values
# so there can be spatial trend in data. 
# e.g. city centres there can be more traffic flow
# e.g. low traffic flow during night time



# =======================================================
# ESpatialDA via 2d and 3d visualization
# =======================================================
# looking at data at spatial and temporal scales
# matrix of scatterplots showing the relationship between mean tf per station
long <- df_meta$longitude
lat <- df_meta$latitude
pairs(~long+lat+rowMeans(traf_matrix),
      main="Simple Scatterplot Matrix on traffic flow")
# implications: south eastern part of the city have lower traffic flows

# explore further by 3D plotting
# south eastern part of the city having less traffic flows
library(scatterplot3d)
library(plot3D)
# library(rgl)

#1. Using Scatterplot3d
scatterplot3d(x=df$long, y=df$lat, z=rowMeans(traf_matrix))
#2. Using plot3D (x,y and z are first three variables so no need to explicitly define them).
scatter3D(x=df$long, y=df$lat, z=rowMeans(traf_matrix))
# interactive in rgl
# plot3d(x=df$long, y=df$lat, z=rowMeans(traf_matrix))


# =======================================================
# ETemporalDA
# =======================================================

# ======================================================================
# plot mean value over time series
plot(colMeans(traf_matrix), xlab = "Timeline", ylab = "Traffic", type="l", xaxt="n")
x_labels <- seq(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2022-01-31 23:00:00"), by='1 day')
x_ticks <- seq(0,750,25)
axis(1, at = x_ticks, labels=x_labels)
# a regular fluctuation in temporal aspect can be observed.

# ======================================================================
# to show multiple time series
library(lattice)
library(reshape)

# create variables for melt
station <- df$X
traffic_flow <- cbind(station, df)
hour <- seq(from = as.POSIXct("2022-01-01 00:00"),
            to = as.POSIXct("2022-01-31 23:00"), 
            by = "hour")

traf_mat <- data.matrix(traffic_flow[,5:ncol(traffic_flow)])

new_traf <- melt(df, id.vars=1:4, measure.vars=5:ncol(df))
colnames(new_traf)[5:6] <- c("hour", "traffic_flow") 
station.chosen=c("354","616","634","711")
#Create a variable containing just the selected stations

a <- newtemp[station %in% station.chosen,]
xyplot(traffic_flow ~ hour | station, xlab = "hour", type = "l",
       layout = c(5, 2),
       data=a,
       main = "Traffic flow at locations in Salt Lake City")


# =================================================================
# creating heat maps that shows both spatial and temporal aspects
hours <- seq(from = as.POSIXct("2022-01-01 00:00"),
            to = as.POSIXct("2022-01-31 23:00"), 
            by = "hour")
hours_label <- as.character(hours)
hours_label <- substr(hours_label, start=6, stop=13)
colnames(traf_matrix) <- hours_label
rownames(traf_matrix) <- df$X

heatmap(traf_matrix,
        Rowv=NA,
        Colv=NA,
        col=heat.colors(256),
        scale="column", 
        margins=c(5,3),
        #xlab="Hour",
        ylab="Station ID", 
        cexCol=1.1,y.scale.components.subticks(n=10))

# =================================================================
# ordering the stations by location

# order by from West to East
station_ordered<-df[order(df$long, decreasing=FALSE),]
traf_longorder_mat<-data.matrix(station_ordered[,5:ncol(df)])

colnames(traf_longorder_mat) <- hours_label
rownames(traf_longorder_mat) <- station_ordered$X

heatmap(traf_longorder_mat,
        Rowv=NA,
        Colv=NA, 
        col=heat.colors(256),
        scale="column",
        margins=c(3,3),
        ylab="Station ID", 
        #cexCol=1.1,y.scale.components.subticks(n=10)
        )

# order by from North to South
station_ordered<-df[order(df$lat, decreasing=TRUE),]
traf_latorder_mat<-data.matrix(station_ordered[,5:ncol(df)])

colnames(traf_latorder_mat) <- hours_label
rownames(traf_latorder_mat) <- station_ordered$X

heatmap(traf_latorder_mat,
        Rowv=NA,
        Colv=NA, 
        col=heat.colors(256),
        scale="column",
        margins=c(3,3),
        ylab="Station ID", 
        #cexCol=1.1,y.scale.components.subticks(n=10)
        )

# level plot
library(lattice)

levelplot(t(traf_latorder_mat), aspect="fill")
# does not give better illustration, so the heatmaps will be adopted


# =======================================================
# Data Mapping
# =======================================================








