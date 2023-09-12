# Load Libraries
library(raster)
library("lidR")
library("raster")
library(terra)
library(mmand)
library(sf)

setwd("M:\\lidar\\Test\\cloud_compare\\")

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
crs_chm <- crs(chm_canopy)
chm_raster <- as.array(chm_final)
chm_raster[is.na(chm_raster)] <- 0

chm <- raster(chm_raster[,,1])

chm[chm < 0] <- 0
extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
# writeRaster(chm, "M:\\lidar\\Test\\chm\\chm.tif", overwrite = TRUE)

# chm_raster <- as.array(chm_final)
# 
# # Morphological Operations
# k <- shapeKernel(c(3,3), type="disc")
# close_chm <- closing(chm_raster, k)
# opening_chm <- opening(close_chm, k)
# 
# # Handling infinite values
# opening_chm[is.infinite(opening_chm)] <- 0
# chm <- raster(opening_chm[,,1])

# Define the extent and CRS
# extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
# crs(chm) <- crs_chm

calculateSlopeBreaks <- function(rasterObj) {
  rows <- nrow(rasterObj)
  cols <- ncol(rasterObj)
  
  # Resulting raster with custom window sizes
  windowRaster <- raster(nrows = rows, ncols = cols)
  
  # Define directions for the eight cardinal directions
  directions <- list(c(0,1), c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1), c(-1,0), c(-1,1))
  
  for (r in 1:rows) {
    for (c in 1:cols) {
      
      centralPixelValue <- rasterObj[r, c]
      slopeBreaks <- numeric(length(directions))
      
      isEdgePixel <- (r == 1 || r == rows || c == 1 || c == cols)
      allGreater <- TRUE  # Initialize to check if all surrounding pixels are greater
      
      isTreeEdgePixel <- FALSE  # Initialize the flag
      
      for (d in 1:length(directions)) {
        direction <- directions[[d]]
        r_offset <- direction[1]
        c_offset <- direction[2]
        
        minPixelValue <- centralPixelValue
        minStep <- 0
        bufferCount <- 0  # To count the number of pixels that confirm an increase
        
        r_step <- r + r_offset
        c_step <- c + c_offset
        if (r_step >= 1 && r_step <= rows && c_step >= 1 && c_step <= cols) {
          pixelValue <- rasterObj[r_step, c_step]
          
          if (pixelValue == 0) {
            isTreeEdgePixel <- TRUE
            break
          }
        }
        # Move in the direction to find the minimum value
        for (step in 1:max(rows, cols)) {
          r_step <- r + step * r_offset
          c_step <- c + step * c_offset
          
          if (r_step < 1 || r_step > rows || c_step < 1 || c_step > cols) break
          
          pixelValue <- rasterObj[r_step, c_step]
          
          # Skip if the pixel value is zero
          # if (centralPixelValue == 0) {
          #   next
          # }
          
          # Check if the surrounding pixel is greater than the central pixel
          if (pixelValue <= centralPixelValue) {
            allGreater <- FALSE
          }
          
          if (pixelValue < minPixelValue) {
            minPixelValue <- pixelValue
            minStep <- step
            
          } else {
            bufferCount <- bufferCount + 1  # Increase the buffer count
            
            # If the buffer count reaches a threshold (e.g., 3), break the loop
            if (bufferCount >= 3) {
              break
            }
          }
        }
        
        # Store the number of steps it took to find the minimum value pixel
        slopeBreaks[d] <- minStep
      }
      if (isTreeEdgePixel) {
        windowSize <- 0
      }
      # Special case for edge pixels
      if (isEdgePixel && allGreater) {
        windowSize <- 0
      } else {
        windowSize <- mean(slopeBreaks)
      }
      
      windowRaster[r, c] <- windowSize
    }
  }
  
  return(windowRaster)
}

# Example usage:
rasterObj <- raster(matrix(runif(100), 10, 10))

windowSizes <- calculateSlopeBreaks(chm)
crs(windowSizes) <- crs_chm
extent(windowSizes) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
writeRaster(windowSizes, "M:\\lidar\\Test\\result\\test_correct_code1.tif", overwrite = TRUE)