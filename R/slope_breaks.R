# Load Libraries
library(raster)
library("lidR")
library("raster")
library(mmand)
library(sf)

# Read LAS file and set CRS
las <- readLAS("thousand.laz")
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

# Define the extent and CRS
extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
chm[chm < 0] <- 0

# Function to calculate slope breaks
calculateSlopeBreaks <- function(rasterObj) {
  rows <- nrow(rasterObj)
  cols <- ncol(rasterObj)
  
  # Create a raster object to store the window sizes
  windowRaster <- raster(nrows = rows, ncols = cols)
  
  # Define the eight cardinal directions
  directions <- list(c(0,1), c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1), c(-1,0), c(-1,1))
  
  # Loop through each pixel in the raster object
  for (r in 1:rows) {
    for (c in 1:cols) {
      
      # Get the value of the current central pixel
      centralPixelValue <- rasterObj[r,c]
      slopeBreaks <- numeric(length(directions))
      
      # Loop through the cardinal directions
      for (d in 1:length(directions)) {
        direction <- directions[[d]]
        r_offset <- direction[1]
        c_offset <- direction[2]
        
        # Move in the current direction until a lower value is found
        for (step in 1:max(rows, cols)) {
          r_step <- r + step * r_offset
          c_step <- c + step * c_offset
          
          # Check bounds of the raster
          if (r_step < 1 || r_step > rows || c_step < 1 || c_step > cols) break
          
          # Get the value of the neighboring pixel
          pixelValue <- rasterObj[r_step, c_step]
          
          # Check if a break in slope is found
          if (pixelValue < centralPixelValue) {
            slopeBreaks[d] <- step
            break
          }
        }
      }
      
      # Calculate the mean value of breaks or assign zero if none found
      windowSize <- if (all(slopeBreaks == 0)) 0 else mean(slopeBreaks[slopeBreaks > 0])
      windowRaster[r, c] <- windowSize
    }
  }
  
  return(windowRaster)
}


# Apply the slope breaks calculation
windowSizes <- calculateSlopeBreaks(chm)
crs(windowSizes) <- crs_chm
extent(windowSizes) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)

# Save and plot
writeRaster(windowSizes, "thousand.tif")
plot(windowSizes)


