library("lidR")
library("raster")
library(rgl)
library(dplyr)
library(RANN)
library(terra)
library(sf)
library(mmand)
library(ggplot2)
library(sp)

setwd("M:\\lidar\\Test\\cloud_compare\\")

# Read LAS file and set CRS
las <- readLAS("braunsmallClean.las")
st_crs(las) <- 25832
chm_canopy <- rasterize_canopy(las, res = 0.5, p2r(0.6),na.fill = kriging())

gf_0.3 <- focalWeight(chm_canopy, 0.3, "Gauss")
chm_smooth_0.3 <- focal(chm_canopy, w = gf_0.3)

gf_0.4 <- focalWeight(chm_canopy, 0.4, "Gauss")
chm_smooth_0.4 <- focal(chm_canopy, w = gf_0.4)

chm_final <- ifel(chm_canopy <= 5, chm_smooth_0.3, chm_smooth_0.4)

crs_chm <- crs(chm_canopy)

# k <- shapeKernel(c(3,3), type="disc")
# close_chm <- mmand::closing(chm_array, k)
# opening_chm <- mmand::opening(close_chm, k)
# # Identify and replace infinite values with NA
# opening_chm[is.infinite(opening_chm)] <- 0
# 
# chm <- raster(opening_chm[,,1])
chm_raster <- as.array(chm_final)
chm_raster[is.na(chm_raster)] <- 0

chm <- raster(chm_raster[,,1])

chm[chm < 0] <- 0
crs_chm <- crs(chm)

extent(chm) <- c(las@header$`Min X`,las@header$`Max X`,las@header$`Min Y`, las@header$`Max Y`)
crs(chm) <- crs_chm
chm[chm < 0] <- 0
# writeRaster(chm,"M:\\lidar\\Test\\result\\braun_chm.tif", overwrite = TRUE)
max_value <- maxValue(chm)


f3 <- function(x) {
  y <- ifelse(x <= 12,
              -0.00115*x^2 + 0.42936*x + 1.16981,
              ifelse(x > 12 & x <= 20,
                     0.00657*x^2 + 0.05479*x,  
                     -0.00115*x^2 + 0.42936*x + 1.16981))
  
  return(y)
}
heights <- seq(1, 2, 0.5)

# Initialize an empty dataframe for results
df_comb <- data.frame()

datalist <- list()
check <- datalist[1]

# Define the window sizes
ws <- 1:max_value

# Iterate through each window size
for (i in ws) {
  
  # Find tree tops using local maxima filter with varying window size
  ttops <- locate_trees(chm, lmf(as.numeric(i), shape="square"))
  
  # Add a new column for the window size
  ttops$ws <- i
  datalist[[i]] <- ttops
  
}

datalist[sapply(datalist, is.null)] <- NULL

# Display the 100th row
df <- dplyr::bind_rows(datalist)

# for (index in seq_along(datalist)) {
#   base_path <- "M:\\lidar\\Test\\diff_window_size\\slopebreak2\\"
#   proposed_file_name <- paste0(base_path, datalist[[index]]$ws[1], ".shp")
# 
#   if (file.exists(proposed_file_name)){
#     print(paste("File", proposed_file_name, "already exists"))
#   } else {
#     df <- datalist[[index]]
#     sf::st_write(df, proposed_file_name, layer_options = "SHPT=POINTZ")
#   }
# }

r<- raster("M:\\lidar\\Test\\result\\test_correct_code.tif")

spts <- rasterToPoints(r, spatial = TRUE)
df_raster <- data.frame(spts@coords)

# Convert to dataframe and add pixel values as 'ws' column
df_raster <- data.frame(spts@coords, ws = spts@data[,1])

df_raster <- df_raster[df_raster$ws > 0,]
df_raster$ws <- round((df_raster$ws))

df_ttops<- data.frame(st_coordinates(df$geometry), ws = df$ws)

df_ttops <- df_ttops %>% 
  rename(
    x = X,
    y = Y,
    z = Z
  )


# Joining the dataframes based on 'x' and 'y'
result <- df_ttops %>%
  inner_join(df_raster, by = c("x", "y")) %>%
  # Filtering rows where 'ws' values are the same in both dataframes
  filter(ws.x == ws.y) %>%
  # Selecting only 'x', 'y', and 'z' from df_ttops
  select(x, y, z)

df_filtered <- data.frame(result)

# # First, sort the data frame by 'z' values
# df <- df[order(df$z),]
# 
# # Group the data by 'z' and then apply a custom function to filter duplicates within 3 meters
# df_filtered <- df %>%
#   group_by(z) %>%
#   arrange(x, y) %>%  # Optional: arranging data can make distance calculation more efficient
#   mutate(keep = row_number() == 1 | # Keep the first occurrence
#            sqrt((x - lag(x))^2 + (y - lag(y))^2) >= 3) %>%  # Distance condition
#   ungroup() %>%
#   filter(keep) %>%
#   select(-keep)  # Remove the 'keep' column

coordinates(df) <- ~x + y

# ttops_2d <- st_zm(ttops, z = NULL, m = NULL)  # Convert 3D points to 2D
# Save treetops as a point shapefile
treetops_sf <- sf::st_as_sf(df)
st_crs(treetops_sf) <- 25832
sf::st_write(treetops_sf, "M:\\lidar\\Test\\points\\test_correct_code1.shp", overwrite = TRUE)