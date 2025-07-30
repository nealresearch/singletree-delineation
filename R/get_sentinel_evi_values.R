library(raster)
library(terra)
library(lubridate)

get_sentinel_value <- function(path, x, y){
  
  # --- Define helper function to extract pixel values ---
  getPixelValues <- function(raster, x, y) {
    values <- terra::extract(raster, cbind(x, y))
    return(values)
  }
  # --- Define function to check if file is from summer (May–Aug) ---
  is_summer_file <- function(filename) {
    date_part <- strsplit(filename, "_")[[1]][2]             # "2018-07-14Z.tif"
    date_clean <- strsplit(date_part, "Z")[[1]][1]           # "2018-07-14"
    month_num <- as.numeric(strsplit(date_clean, "-")[[1]][2]) # Extract "07" -> 7
    return (month_num %in% 1:12)
  }
  
  # Path to the ndvi files
  evi_path <- path
  evi_files_all <- list.files(evi_path)

  # --- Filter for summer files only ---
  summer_idx <- sapply(evi_files_all, is_summer_file)
  print(summer_idx)
  # evi_files <- evi_files_all[summer_idx]
  evi_files <- evi_files_all 
  # --- Load and extract pixel values ---
  r1_l <- list()
  dates <- c()
  dates <- as.Date(character(length(evi_files)))  # initialize Date vector
  
  for (i in seq_along(evi_files)) {
    evi_file <- evi_files[i]
    # Extract date
    date_str <- strsplit(evi_file, "_")[[1]][3]
    date_clean <- strsplit(date_str, "Z")[[1]][1]
    dates[i] <- as.Date(date_clean)  # ensure this stays Date class
    # Load rasters and extract values
    r1 <- raster(paste0(evi_path, evi_file))

    r1_l[[i]] <- getPixelValues(r1, x, y)
    print(getPixelValues(r1, x, y))
  }
  # --- Convert to vectors and clean ---
  evi_values <- unlist(r1_l)
  return (evi_values)
}
