library(raster)
library(foreach)
library(doParallel)
library(stringr)

endmember_correction <- function(rast_path, image, unmix_path, unmix_image, grass_values, building_values, tree_values){

  get_group_number <- function(month_val, month_groups) {
    match_found <- sapply(month_groups, function(group) month_val %in% group)
    group_name <- names(match_found)[match_found]
    if (length(group_name) > 0) {
      return(as.integer(group_name))
    } else {
      return(NA)
    }
  }
  month_groups <- list(
    "1" = c(1, 2, 3, 4),
    "2" = c(5),
    "3" = c(6, 7),
    "4" = c(8, 9),
    "5" = c(10, 11, 12)
  )
  
  name <- stringr::str_split(image, pattern = "_")[[1]]
  d <- stringr::str_split(name[[3]], pattern = "Z")
  date <- d[[1]][1]
  years <- stringr::str_split(date, pattern = "-")
  
  year <- years[[1]][1]
  month <- years[[1]][2]
  
  group_number <- get_group_number(as.numeric(month), month_groups)
  group_number <- as.character(group_number)
  
  tree_endmember <- tree_values[year][[1]][group_number]
  grass_endmember <- grass_values[year][[1]][group_number]
  building_endmember <- building_values [year][[1]][group_number]
  
  if (is.null(tree_endmember[[1]])) {
    tree_endmember <- tree_val["2018"][[1]][group_number]
  }
  if (is.null(grass_endmember[[1]])) {
    grass_endmember <- grass_val["2018"][[1]][group_number]
  }
  if (is.null(building_endmember[[1]])) {
    building_endmember <- building_val["2018"][[1]][group_number]
  }
  
  img_raster <- stack(file.path(rast_path, image))
  unmix_raster <- stack(file.path(unmix_path, unmix_image))

  F_grass<- unmix_raster[[1]]
  F_building <- unmix_raster[[2]]
  F_trees <- unmix_raster[[3]]
  
  # helper – returns the 10 numeric band values, skipping ID
  get_band_vals <- function(x) as.numeric(x[[1]][-1])   # removes the first element
  tree_vals  <- get_band_vals(tree_endmember)
  grass_vals <- get_band_vals(grass_endmember)
  build_vals <- get_band_vals(building_endmember)
  
  # build the 10 tree-only bands
  corrected_bands <- lapply(seq_along(tree_vals), function(i) {
    R_mix <- tree_vals[i]  * F_trees +
      grass_vals[i] * F_grass +
      build_vals[i] * F_building
    R_mix - grass_vals[i] * F_grass - build_vals[i] * F_building   # tree component
  })
  corrected_stack <- stack(corrected_bands)
  return (corrected_stack)
}
