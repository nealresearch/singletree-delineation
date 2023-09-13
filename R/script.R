library("lidR")
library("raster")
library(rgl)
library(dplyr)
library(RANN)
library(terra)
library(sf)


lasMainData<-readLAS("M:\\lidar\\RandomForest\\classified_data\\Normalized\\Braunschweig.laz")


las <- filter_poi(lasMainData, Classification == 5)

plot(las)
 