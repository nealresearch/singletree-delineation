library(raster)
library("lidR")
library("raster")
library(mmand)
library(sf)

las <- readLAS("")
plot(las)
st_crs(las) <- 28532

chma <- rasterize_canopy(las, res = 0.5, p2r(0.6),na.fill = kriging())
gf <- focalWeight(chma, .4, "Gauss")
chm_smooth <- focal(chma, w = gf)

crs_chm <- crs(chma)
chm_raster <- as.array(chm_smooth)

k <- shapeKernel(c(3,3), type="disc")

close_chm <- closing(chm_raster, k)
opening_chm <- opening(close_chm, k)



# Identify and replace infinite values with NA
opening_chm[is.infinite(opening_chm)] <- 0

chm <- raster(opening_chm[,,1])

extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
chm[chm < 0] <- 0

calculateSlopeBreaks <- function(rasterObj) {
  rows <- nrow(rasterObj)
  cols <- ncol(rasterObj)
  
  # Resulting raster with custom window sizes
  windowRaster <- raster(nrows = rows, ncols = cols)
  
  # Define directions for the eight cardinal directions
  directions <- list(c(0,1), c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1), c(-1,0), c(-1,1))
  
  for (r in 1:rows) {
    for (c in 1:cols) {
      
      centralPixelValue <- rasterObj[r,c]
      slopeBreaks <- numeric(length(directions))
      
      for (d in 1:length(directions)) {
        direction <- directions[[d]]
        r_offset <- direction[1]
        c_offset <- direction[2]
        
        # Move in the direction until a lower value is found
        for (step in 1:max(rows, cols)) {
          r_step <- r + step * r_offset
          c_step <- c + step * c_offset
          
          if (r_step < 1 || r_step > rows || c_step < 1 || c_step > cols) break
          
          pixelValue <- rasterObj[r_step, c_step]
          if (pixelValue < centralPixelValue) {
            slopeBreaks[d] <- step
            break
          }
        }
      }
      
      # Calculate mean value for the window size, or assign zero if no breaks were found
      windowSize <- if (all(slopeBreaks == 0)) 0 else mean(slopeBreaks[slopeBreaks > 0])
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
writeRaster(windowSizes, "")
plot(windowSizes)