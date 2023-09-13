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
mean_height <- mean(las$Z)

st_crs(las) <- 25832

chma <- rasterize_canopy(las, res = 0.5, p2r(0.3))
gf <- focalWeight(chma, .3, "Gauss")
chm_smooth <- focal(chma, w = gf)
crs_chm <- crs(chma)
chm_raster <- as.array(chm_smooth)

k <- shapeKernel(c(3,3), type="disc")
dilate_chm <- closing(chm_raster, k)
erode_chm <- opening(dilate_chm, k)
# Identify and replace infinite values with NA
erode_chm[is.infinite(erode_chm)] <- NA

chm <- raster(erode_chm[,,1])

extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
tree_cadaster_data <- read.csv("M:\\lidar\\Test\\thousandtrees\\thousand_trees.csv")
tree_cadaster <- length(tree_cadaster_data$OBJECTID)
initial_height <- 1
second_height <- 3
poly_length <- list()

while (second_height < 10){
  f3 <- function(x) {
    y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
    y[x <= mean_height] <- initial_height
    y[x > mean_height] <- second_height
    return(y)
  }
  heights <- seq(0 ,40, 0.5)
  ws <- f3(heights)
  # x<- plot(las,bg="white")
  ttops <- locate_trees(chm, lmf(f3, shape="circular"))
  # plot(chm, col = height.colors(50))
  # plot(sf::st_geometry(ttops), add = TRUE, pch = 1)
  # add_treetops3d(x, ttops)
  ttops_df <- st_set_geometry(ttops, NULL)
  ttops_coords <- st_coordinates(ttops)[, 1:2] # assuming columns 1 and 2 are your X and Y coordinates
  ttops_sp <- SpatialPointsDataFrame(ttops_coords, ttops_df)
  mcws_segments_grid <- (ForestTools::mcws(ttops_sp, chm, format="POLYGONS"))
  
  crowns_number <- length(mcws_segments_grid)
  poly_length <- append(poly_length, list(list(crowns_number, initial_height, second_height)))
  
  initial_height <- initial_height + 1
  second_height <- second_height + 1
}

crown_df <- do.call(rbind, poly_length)
crown_df <- data.frame(crown_df)
names(crown_df) <- c("crowns_number", "initial_height", "second_height")
transform(crown_df,window_size=paste0(crown_df$initial_height,crown_df$second_height))

# Make sure data types are correct
crown_df$crowns_number <- as.numeric(as.character(crown_df$crowns_number))
crown_df$initial_height <- as.numeric(as.character(crown_df$initial_height))
crown_df$second_height <- as.numeric(as.character(crown_df$second_height))

grp <- ggplot(crown_df, aes(x = initial_height, y = crowns_number)) +
  geom_line() +
  labs(x = "window_size", y = "Tree Number", title = "Scatter plot of Tree Number vs window_size")

# Add the tree_number_cadaster as a single point at maximum tolerance
p <- grp + geom_point(aes(x = max(initial_height), y = tree_cadaster), colour = "red", size = 3)

p + scale_x_continuous(breaks=c(seq(1, 10, 1))) + scale_y_continuous(breaks = seq(10, 7000, by = 200))
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
mean_height <- mean(las$Z)

st_crs(las) <- 25832

chma <- rasterize_canopy(las, res = 0.5, p2r(0.3))
gf <- focalWeight(chma, .3, "Gauss")
chm_smooth <- focal(chma, w = gf)
crs_chm <- crs(chma)
chm_raster <- as.array(chm_smooth)

k <- shapeKernel(c(3,3), type="disc")
dilate_chm <- closing(chm_raster, k)
erode_chm <- opening(dilate_chm, k)
# Identify and replace infinite values with NA
erode_chm[is.infinite(erode_chm)] <- NA

chm <- raster(erode_chm[,,1])

extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
tree_cadaster_data <- read.csv("M:\\lidar\\Test\\thousandtrees\\thousand_trees.csv")
tree_cadaster <- length(tree_cadaster_data$OBJECTID)
initial_height <- 1
second_height <- 3
poly_length <- list()

while (second_height < 10){
  f3 <- function(x) {
    y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
    y[x <= mean_height] <- initial_height
    y[x > mean_height] <- second_height
    return(y)
  }
  heights <- seq(0 ,40, 0.5)
  ws <- f3(heights)
  # x<- plot(las,bg="white")
  ttops <- locate_trees(chm, lmf(f3, shape="circular"))
  # plot(chm, col = height.colors(50))
  # plot(sf::st_geometry(ttops), add = TRUE, pch = 1)
  # add_treetops3d(x, ttops)
  ttops_df <- st_set_geometry(ttops, NULL)
  ttops_coords <- st_coordinates(ttops)[, 1:2] # assuming columns 1 and 2 are your X and Y coordinates
  ttops_sp <- SpatialPointsDataFrame(ttops_coords, ttops_df)
  mcws_segments_grid <- (ForestTools::mcws(ttops_sp, chm, format="POLYGONS"))
  
  crowns_number <- length(mcws_segments_grid)
  poly_length <- append(poly_length, list(list(crowns_number, initial_height, second_height)))
  
  initial_height <- initial_height + 1
  second_height <- second_height + 1
}

crown_df <- do.call(rbind, poly_length)
crown_df <- data.frame(crown_df)
names(crown_df) <- c("crowns_number", "initial_height", "second_height")
transform(crown_df,window_size=paste0(crown_df$initial_height,crown_df$second_height))

# Make sure data types are correct
crown_df$crowns_number <- as.numeric(as.character(crown_df$crowns_number))
crown_df$initial_height <- as.numeric(as.character(crown_df$initial_height))
crown_df$second_height <- as.numeric(as.character(crown_df$second_height))

grp <- ggplot(crown_df, aes(x = initial_height, y = crowns_number)) +
  geom_line() +
  labs(x = "window_size", y = "Tree Number", title = "Scatter plot of Tree Number vs window_size")

# Add the tree_number_cadaster as a single point at maximum tolerance
p <- grp + geom_point(aes(x = max(initial_height), y = tree_cadaster), colour = "red", size = 3)

p + scale_x_continuous(breaks=c(seq(1, 10, 1))) + scale_y_continuous(breaks = seq(10, 7000, by = 200))
>>>>>>> f434241dbbe65857c8627be1f1aca2955599771a
