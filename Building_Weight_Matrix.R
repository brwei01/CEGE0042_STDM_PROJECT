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
# Load data
# ===============================================================
current_directory <- getwd()
setwd(current_directory)
df <- read.table('Data/C_Utah_Traffic.csv', header = TRUE, sep = ",")

# ===============================================================
# building weight matrix by distanceband/KNN
# ===============================================================
stations <- data.frame(cbind(df$long, df$lat))
colnames(stations) <- c('easting','northing')
coords <- SpatialPoints(stations)
points <- coords

# check for duplicates
# duplicates <- duplicated(stations$easting)
# stations[duplicates,]

proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
coords <- spTransform(coords, CRS("+proj=utm +zone=12 +datum=WGS84"))
coords <- data.frame(coords@coords)
coords <- cbind(coords$easting, coords$northing)

# duplicates <- duplicated(coords)
# coords[duplicates,]

coords <- as.matrix(coords)
class(coords)



# ===============================================================
# show points on map
# ===============================================================
tmap_mode("view")
tm_shape(points) + tm_dots()
tmap_mode('plot')


# ===============================================================
# Weight Matrix by Distance Band
# adapted from https://spatialanalysis.github.io/lab_tutorials/Distance_Based_Spatial_Weights.html#distance-band-weights
# ===============================================================
# computing critical threshold
knn1 <- knearneigh(coords)
# view knn results
str(knn1)
k1 <- knn2nb(knn1)
# computing critical threshold
critical.threshold <- max(unlist(nbdists(k1, coords)))
critical.threshold

# computing distance-band weights
nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
nb.dist.band <- dnearneigh(coords, 0, critical.threshold+3000)
summary(nb.dist.band)

# plot connectivity histogram
dist.band.card <- card(nb.dist.band)
dist.band.card
ggplot() +
  geom_histogram(aes(x=dist.band.card)) +
  xlab("Number of Neighbors")

# plot connectivity graph
plot(nb.dist.band, coords, lwd=.2, col="blue", cex = .5)

# convert nb.dist.band to matrix
nb_mat <- nb2mat(nb.dist.band)

# save matrix
write.csv(nb_mat, file='Data/WD_Utah_Traffic.csv')



# ===============================================================
# by KNN (DO NOT NEED TO RUN THIS SECTION)
# ===============================================================
# assuming that every node connects to 4 roads
k <- knn2nb(knearneigh(coords, k=4))
plot(k, coords, lwd=.2, col="blue", cex = .5)

# convert nb to matrix
k_mat <- nb2mat(k)

# save weight matrix
write.csv(k_mat, file='Data/W_Utah_Traffic.csv')