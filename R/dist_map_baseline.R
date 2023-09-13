<<<<<<< HEAD
library("lidR")
library("raster")
library(rgl)
library(dplyr)
library(RANN)
library(terra)
library(sf)
library(mmand)
library(ggplot2)

las <- readLAS("M:\\lidar\\Test\\thousandtrees\\thousand.laz")

st_crs(las) <- 25832

f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x >3 ] <- 3
  return(y)
}
heights <- seq(0 ,40, 0.5)
ws <- f(heights)

chma <-
  rasterize_canopy(las, res = 0.5, p2r(0.3, na.fill = NULL))
gf <- focalWeight(chma, .3, "Gauss")
chma <- focal(chma, w = gf)

# Set the CRS of the raster
terra::crs(chma) <- "+init=epsg:25832"
crs_chm <- crs(chma)
chm_raster <- as.array(chma)

k <- shapeKernel(c(3,3), type="disc")

close_chm <- closing(chm_raster, k)
opening_chm <- opening(close_chm, k)

# Identify and replace infinite values with NA
opening_chm[is.infinite(opening_chm)] <- NA

# Convert to raster
chm <- raster(opening_chm[,,1])
extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
plot(chm)
tree_cadaster <- read.csv("M:\\lidar\\Test\\thousandtrees\\thousand_trees.csv")


tree_number_cadaster <- length(tree_cadaster$OBJECTID)
tree_number_cadaster
tolerance = 0.1
crown_number = list()
crown_area = list()

while (tolerance <= 2){  
  
  lasSegmentTrees <-segment_trees(las, watershed(chm, th_tree = 1, tol = tolerance, ext = 1))
  
  lasFilterZero <- filter_poi(lasSegmentTrees, !(is.na(treeID)))
  
  crowns <- crown_metrics(lasFilterZero, func = .stdtreemetrics, geom = "concave")
  
  crowns_number <- length(crowns$convhull_area)
  
  if (crowns_number == tree_number_cadaster){
    print(tolerance)
  }
  crowns_area <-crowns$convhull_area
  
  crown_number <- append(crown_number, list(list(crowns_number, tolerance)))
  
  crown_area <- append(crowns_area, list(list(crowns_area, tolerance)))
  
  tolerance <- tolerance + 0.01
}

crown_number_df <- do.call(rbind, crown_number)
crown_number_df <- data.frame(crown_number_df)
names(crown_number_df) <- c("crowns_number", "tolerance")

# Make sure data types are correct
crown_number_df$crowns_number <- as.numeric(as.character(crown_number_df$crowns_number))
crown_number_df$tolerance <- as.numeric(as.character(crown_number_df$tolerance))

tree_number_cadaster_series <- rep(tree_number_cadaster, length(crown_number_df$crowns_number))


grp <- ggplot(crown_number_df, aes(x = tolerance, y = crowns_number)) +
  geom_line() +
  labs(x = "Tolerance", y = "Tree Number", title = "Scatter plot of Tree Number vs Tolerance")

# Add the tree_number_cadaster as a single point at maximum tolerance
p <- grp + geom_point(aes(x = max(tolerance), y = tree_number_cadaster), colour = "red", size = 3)

p + scale_x_continuous(breaks=c(seq(0.1, 0.6, 0.2), seq(0.1, 5, 0.5))) + scale_y_continuous(breaks = seq(0, tree_number_cadaster + 20, by = 25))
=======
library("lidR")
library("raster")
library(rgl)
library(dplyr)
library(RANN)
library(terra)
library(sf)
library(mmand)
library(ggplot2)

las <- readLAS("M:\\lidar\\Test\\thousandtrees\\thousand.laz")

st_crs(las) <- 25832

f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x >3 ] <- 3
  return(y)
}
heights <- seq(0 ,40, 0.5)
ws <- f(heights)

chma <-
  rasterize_canopy(las, res = 0.5, p2r(0.3, na.fill = NULL))
gf <- focalWeight(chma, .3, "Gauss")
chma <- focal(chma, w = gf)

# Set the CRS of the raster
terra::crs(chma) <- "+init=epsg:25832"
crs_chm <- crs(chma)
chm_raster <- as.array(chma)

k <- shapeKernel(c(3,3), type="disc")

close_chm <- closing(chm_raster, k)
opening_chm <- opening(close_chm, k)

# Identify and replace infinite values with NA
opening_chm[is.infinite(opening_chm)] <- NA

# Convert to raster
chm <- raster(opening_chm[,,1])
extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
plot(chm)
tree_cadaster <- read.csv("M:\\lidar\\Test\\thousandtrees\\thousand_trees.csv")


tree_number_cadaster <- length(tree_cadaster$OBJECTID)
tree_number_cadaster
tolerance = 0.1
crown_number = list()
crown_area = list()

while (tolerance <= 5){  
  
  lasSegmentTrees <-segment_trees(las, watershed(chm, th_tree = 1, tol = tolerance, ext = 1))
  
  lasFilterZero <- filter_poi(lasSegmentTrees, !(is.na(treeID)))
  
  crowns <- crown_metrics(lasFilterZero, func = .stdtreemetrics, geom = "concave")
  
  crowns_number <- length(crowns$convhull_area)
  
  if (crowns_number == tree_number_cadaster){
    print(tolerance)
  }
  crowns_area <-crowns$convhull_area
  
  crown_number <- append(crown_number, list(list(crowns_number, tolerance)))
  
  crown_area <- append(crowns_area, list(list(crowns_area, tolerance)))
  
  tolerance <- tolerance + 0.01
}

crown_number_df <- do.call(rbind, crown_number)
crown_number_df <- data.frame(crown_number_df)
names(crown_number_df) <- c("crowns_number", "tolerance")

# Make sure data types are correct
crown_number_df$crowns_number <- as.numeric(as.character(crown_number_df$crowns_number))
crown_number_df$tolerance <- as.numeric(as.character(crown_number_df$tolerance))

tree_number_cadaster_series <- rep(tree_number_cadaster, length(crown_number_df$crowns_number))


grp <- ggplot(crown_number_df, aes(x = tolerance, y = crowns_number)) +
  geom_line() +
  labs(x = "Tolerance", y = "Tree Number", title = "Scatter plot of Tree Number vs Tolerance")

# Add the tree_number_cadaster as a single point at maximum tolerance
p <- grp + geom_point(aes(x = max(tolerance), y = tree_number_cadaster), colour = "red", size = 3)

p + scale_x_continuous(breaks=c(seq(0.1, 0.6, 0.2), seq(0.1, 5, 0.5))) + scale_y_continuous(breaks = seq(0, tree_number_cadaster + 20, by = 25))
>>>>>>> f434241dbbe65857c8627be1f1aca2955599771a
