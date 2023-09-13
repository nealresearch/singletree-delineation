# Load Libraries
library(raster)
library("lidR")
library("raster")
library(terra)
library(mmand)
library(sf)

setwd("M:\\lidar\\Test\\cloud_compare")

# Read LAS file and set CRS
las <- readLAS("braunsmallClean.las")
st_crs(las) <- 25832

# Canopy rasterization
chm_canopy <- rasterize_canopy(las, res = 0.5, p2r(0.6), na.fill = kriging())

# Gaussian Smoothing with different kernel sizes
gf_0.3 <- focalWeight(chm_canopy, 0.3, "Gauss")
chm_smooth_0.3 <- focal(chm_canopy, w = gf_0.3)
gf_0.4 <- focalWeight(chm_canopy, 0.4, "Gauss")
chm_smooth_0.4 <- focal(chm_canopy, w = gf_0.4)

# Conditional raster smoothing
chm_final <- ifel(chm_canopy <= 5, chm_smooth_0.3, chm_smooth_0.4)
crs_chm <- crs(chm_final) # Fixing a typo
chm_raster <- as.array(chm_final)

# Morphological Operations
k <- shapeKernel(c(3,3), type="disc")
close_chm <- closing(chm_raster, k)
opening_chm <- opening(close_chm, k)

# Handling infinite values
opening_chm[is.infinite(opening_chm)] <- 0
chm <- raster(opening_chm[,,1])
plot(chm)
# Define the extent and CRS
extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
chm[chm < 0] <- 0

rows <- nrow(chm)
cols <- ncol(chm)
canopy_maxima <- matrix(0, nrow = rows, ncol = cols) # Output matrix

# Loop through each row
for (row in 1:rows) {
  # Loop through each column
  for (col in 1:cols) {
    current_pixel <- chm[row, col]
    
    # Skip if pixel value is less or equal to 0 or NA
    if (current_pixel <= 0 | is.na(current_pixel)) {
      next
    }
    
    # Determine window size around current pixel
    half_window <- round(current_pixel / 2)
    window_size <- 2 * half_window + 1
    temp_value <- matrix(0, nrow = window_size, ncol = window_size)
    counter <- 1
    
    # Loop through the window around the current pixel
    for (i in (row - half_window):(row + half_window)) { 
      for (j in (col - half_window):(col + half_window)) {
        # Check if indices are within the image boundaries
        if (i > 0 & j > 0 & i <= rows & j <= cols) {
          temp_value[counter] <- chm[i, j]
          counter <- counter + 1
        }
      }
    }
    
    # Find the maximum value within the window
    max_pixel <- max(temp_value, na.rm = TRUE) 
    
    # Assign the maximum value to the corresponding location in the output
    canopy_maxima[row, col] <- max_pixel 
  }
}

#chm <- as.matrix(chm)
# # Normalizing the canopy_maxima matrix
# canopy_maxima_normalized <- canopy_maxima / max(chm, na.rm = TRUE)

# Converting to a raster object
canopy_maxima_raster <- raster(canopy_maxima)
extent(canopy_maxima_raster) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(canopy_maxima_raster) <- crs_chm
canopy_maxima_raster