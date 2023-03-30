# =======================================================
# load data
# =======================================================
setwd('/Users/apple/Desktop/CEGE0042_R/CEGE0042_STDM_PROJECT')
df <- read.csv('Data/N_Utah_Traffic.csv')
df_meta <- read.csv('Data/N_Utah_Traffic_Meta.csv')
# setting the first col(time) as indexes
row.names(df) <- df$X
df$X <- NULL
names(df) <- substring(names(df),2)


# transpose df and make it into matrix
traf_matrix <- t(df)
# replace the NaN data with mean in the location )

 
# adding lat and long to traf_matrix
station <- as.numeric(rownames(traf_matrix))
long <- df_meta$longitude[station == df_meta$station_id]
lat <- df_meta$latitude[station == df_meta$station_id]
route <- df_meta$route[station == df_meta$station_id]
traf_matrix <- cbind(long, lat, route, traf_matrix)

# =======================================================
# save the cleaned data
# =======================================================
write.csv(traf_matrix, file='Data/C_Utah_Traffic.csv')


# =======================================================
# load the cleaned data
# =======================================================
df <- read.csv('Data/C_Utah_Traffic.csv')
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
# data are not normall distributed, but highly positively skewed

# examining the distribution i QQ plot
library(ggplot2)
qqnorm(traf_matrix)
qqline(traf_matrix, col='red')
# explanations to this might be: a lot of stations have low traffic values
# so there can be spatial trend in data. 
# e.g. city centres there can be more traffic flow
# e.g. low traffic flow during night time



# =======================================================
# spatial EDA via 2d and 3d visualization
# =======================================================
# looking at data at spatial and temporal scales
# matrix of scatterplots showing the relationship between mean tf per station

pairs(~long+lat+rowMeans(traf_matrix),
      main="Simple Scatterplot Matrix on traffic flow")
# implications


# explore further by 3D plotting
# south eastern part of the city having less traffic flows
library(scatterplot3d)
library(plot3D)
library(rgl)

#1. Using Scatterplot3d
scatterplot3d(x=df$long, y=df$lat, z=rowMeans(traf_matrix))
#2. Using plot3D (x,y and z are first three variables so no need to explicitly define them).
scatter3D(x=df$long, y=df$lat, z=rowMeans(traf_matrix))
# interactive in rgl
plot3d(x=df$long, y=df$lat, z=rowMeans(traf_matrix))


# =======================================================
# explore temporal aspects
# =======================================================
plot(colMeans(traf_matrix), xlab = "Timeline", ylab = "Traffic", type="l", xaxt="n")
x_labels <- seq(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2022-01-31 23:00:00"), by='1 day')
x_ticks <- seq(0,750,25)
axis(1, at = x_ticks, labels=x_labels)
# a regular fluctuation in temporal aspect can be observed.




# =======================================================
# data mapping
# =======================================================




# =======================================================
# view the lagged variables
# =======================================================
UTMeanTraf <- colMeans(df[,5:ncol(df)])
UTLagged <- data.frame(hour=as.POSIXct('2022-01-01 00:00:00'):as.POSIXct())




