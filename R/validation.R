library(sf)
library(lidR)
library(dplyr)
library(terra)

# --- Load Data ---
points <- st_read("//home//nilraj.shrestha//R//paper1//filter_baumcadaster.shp")
poly   <- st_read("//home//nilraj.shrestha//R//paper1//hamburg_polygon.shp")

chm1 <- rast("//home//nilraj.shrestha//R//lasdata//digi_twin//hamburg1.tiff")
chm2 <- rast("//home//nilraj.shrestha//R//lasdata//digi_twin//hamburg2.tiff")
chm3 <- rast("//home//nilraj.shrestha//R//lasdata//digi_twin//hamburg4.tiff")

# Merge CHM tiles
chm_canopy <- terra::merge(chm1, chm2, chm3)
terra::crs(chm_canopy) <- crs(points)
names(chm_canopy) <- "Z"

# --- 1. Find polygons that have at least one point ---
point_in_poly <- st_intersects(points, poly)
poly_ids_with_points <- unique(unlist(point_in_poly))
poly_with_points <- poly[poly_ids_with_points, ]

tree_ids <- integer(length(point_in_poly))  # numeric placeholder

# fill with NA initially
tree_ids[] <- NA

# assign treeID for each point
for (i in seq_along(point_in_poly)) {
  idx <- point_in_poly[[i]]
  if (length(idx) > 0) {
    tree_ids[i] <- poly$treeID[idx[1]]   # take the first polygon match
  }
}

# add as new field in "points"
points$treeID_poly <- tree_ids

# --- 2. Extract max CHM (x, y, Z) for each polygon ---
poly_vect <- vect(poly_with_points)
vals <- terra::extract(chm_canopy, poly_vect, cells = TRUE, xy = TRUE)

max_xyz_df <- vals |>
  group_by(ID) |>
  slice_max(order_by = Z, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(ID, x, y, Z)

max_points <- st_as_sf(max_xyz_df, coords = c("x", "y"), crs = st_crs(poly))

# --- 3. Replace point geometries with CHM maxima ---
updated_points <- points
poly_matches <- st_intersects(points, poly_with_points)

for (i in seq_along(poly_matches)) {
  if (length(poly_matches[[i]]) == 0) next
  poly_id <- poly_matches[[i]]
  
  if (length(poly_id) == 1) {
    st_geometry(updated_points[i, ]) <- st_geometry(max_points[poly_id, ])
  } else {
    chm_pt <- st_geometry(max_points[poly_id, ])
    d <- st_distance(points[i, ], chm_pt)
    if (which.min(d) == 1) {
      st_geometry(updated_points[i, ]) <- chm_pt
    }
  }
}

# --- Buffers ---
buffers  <- st_buffer(updated_points, dist = updated_points$kronendurc / 2)
st_crs(buffers) <- st_crs(updated_points)
buffers
dup_idx <- which(duplicated(st_coordinates(updated_points)) |
                   duplicated(st_coordinates(updated_points), fromLast = TRUE))
updated_points <- updated_points[!duplicated(st_coordinates(updated_points)), ]
buffers <- st_buffer(updated_points, dist = updated_points$kronendurc / 2)

buffers2 <- st_buffer(points, dist = points$kronendurc / 2)

common_cols <- intersect(names(buffers), names(buffers2))
main_buffer <- rbind(buffers[, common_cols], buffers2[, common_cols])

# --- Select final polygons ---
poly$polygon <- lengths(st_contains(poly, points)) > 0

poly_with_points    <- poly[poly$polygon, ]
poly_without_points <- poly[!poly$polygon, ]

intersect_idx <- lengths(st_intersects(poly_without_points, main_buffer)) > 0
poly_touching_buffer <- poly_without_points[intersect_idx, ]

poly_final <- rbind(poly_with_points, poly_touching_buffer)

################################################################################
#                ADD TWO BUFFER INTERSECTION PERCENTAGES HERE                  #
################################################################################

# Function to compute max % overlap
get_max_percent <- function(polys, buffer, fieldname) {
  
  local_poly <- polys
  local_buf  <- buffer
  
  local_poly$poly_id   <- seq_len(nrow(local_poly))
  local_poly$poly_area <- st_area(local_poly)
  local_buf$buffer_id  <- seq_len(nrow(local_buf))
  
  inter_tbl <- st_intersection(
    local_poly[, c("poly_id", "poly_area")],
    local_buf[, "buffer_id"]
  )
  inter_tbl$inter_area <- st_area(inter_tbl)
  
  summary_tbl <- inter_tbl |>
    st_drop_geometry() |>
    group_by(poly_id) |>
    summarise(
      !!fieldname := round(
        max(as.numeric(inter_area) / as.numeric(first(poly_area))), 3
      )
    )
  
  return(summary_tbl)
}

# Compute both percentages
res_buf1 <- get_max_percent(poly_final, buffers,  "max_prcn_buf1")
res_buf2 <- get_max_percent(poly_final, buffers2, "max_prcn_buf2")

# Attach to polygon final
poly_final$poly_id <- seq_len(nrow(poly_final))

poly_final2 <- poly_final |>
  left_join(res_buf1, by = "poly_id") |>
  left_join(res_buf2, by = "poly_id")

# Save result
st_write(poly_final2,
         "//home//nilraj.shrestha//R//paper1//poly_with_twobuf_merge.shp",
         delete_dsn = TRUE)
