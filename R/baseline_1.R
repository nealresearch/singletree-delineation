library(dplyr)
library(purrr)
library(raster)
library(future)
library(tibble)
library(stringr)
library(foreach)
library(doParallel)
library(future.apply)
plan(multisession, workers= 40)
options(digits = 8)

#####################################DATA SOURCE############################################################
source("//home//nilraj.shrestha//R//umixing//R_script//get_group.R")
source("//home//nilraj.shrestha//R//umixing//R_script//get_endmember.R")
source("//home//nilraj.shrestha//R//umixing//R_script//automated_endmember_correction_havel.R")
source("//home//nilraj.shrestha//R//umixing//R_script//get_sentinel_evi_values.R")
source("//home//nilraj.shrestha//R//umixing//R_script//automated_evi.R")

# …and your filepaths + lists
tree_shapefile <- "//home//nilraj.shrestha//R//umixing//Braunschweig//shapefile//trees.shp"
grass_shapefile <- "//home//nilraj.shrestha//R//umixing//Braunschweig//shapefile//grass.shp"
building_shapefile <- "//home//nilraj.shrestha//R//umixing//Braunschweig//shapefile//building.shp"

rast_path  <- "//home//nilraj.shrestha//R//umixing//havel//havel_interpolation_mask//"
unmix_path <- "//home//nilraj.shrestha//R//umixing//havel//havel_unmix_mask//"
sentinel_path <- "//home//nilraj.shrestha//R//umixing//havel//havel_sen_evi_smooth//"
sentinel_path_unsmoothed <- "//home//nilraj.shrestha//R//umixing//havel//masked_sen_evi//"
###########################################################################################################

########################################FUNCTION############################################################
random_number <- function(x, start_numb, end_numb){
  number <- floor(runif(x, min=start_numb, max=end_numb))
  return (number)
}

smooth_pixel <- function(ts){
  if (all(is.na(ts)) || length(unique(ts)) < 3) return(rep(NA, length(ts)))
  q <- quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE)
  w <- ifelse(ts < q[1] | ts > q[2], 0.1, 1)
  
  tryCatch({
    opt <- phenofit::lambda_vcurve(ts, w = w, lg_lambdas = seq(0.1, 1, 0.1))
    lambda <- opt$lambda
    pracma::whittaker(ts, lambda = lambda, d=3)
  }, error=function(e){
    rep(NA, length(ts))
  })
}

# Define smoothing function for a full data frame
smooth_one_df <- function(df) {
  df2 <- df[, !(names(df) %in% c("x", "y"))]
  df_xy <- df[, c("x", "y")]
  band_names <- names(df2)
  
  result_df <- df2 |>
    asplit(1) |>
    map_dfr(\(ts) {
      out <- tryCatch(smooth_pixel(ts), error = function(e) rep(NA, length(ts)))
      names(out) <- band_names
      as_tibble_row(out)
    })
  
  bind_cols(df_xy, result_df)
}

extract_series <- function(df, x0, y0) {
  row <- df[ df$x==x0 & df$y==y0, ]
  if(nrow(row)==0) stop("no data at that point")
  vals <- as.numeric(row[ , -(1:2)])
  dates <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})Z$", "\\1", names(row)[-(1:2)]))
  data.frame(date=dates, value=vals)
}

extract_summer_series <- function(df, x, y) {
  # 1) grab the row
  row <- df[df$x == x & df$y == y, , drop = FALSE]
  if (nrow(row) == 0) stop("no data at that point")
  
  # 2) find just the value-columns
  val_cols <- grep("^val_interpolated_openEO_", names(row), value = TRUE)
  
  # 3) parse their dates
  dates <- as.Date(
    gsub("^val_interpolated_openEO_(.*)Z$", "\\1", val_cols),
    format = "%Y-%m-%d"
  )
  
  # 4) pull the numeric values
  vals <- as.numeric(row[1, val_cols])
  
  # 5) keep only May–August
  months <- as.integer(format(dates, "%m"))
  keep   <- months >= 1 & months <= 12
  
  # 6) return filtered series
  data.frame(
    date  = dates[keep],
    value = vals[keep]
  )
}

#######################################################################################

image_list <- list.files(rast_path)

cat("Found", length(image_list), "images in", rast_path, "\n")
unmix_list <- list.files(unmix_path)
cat("Found", length(unmix_list), "images in", rast_path, "\n")

group <- get_group()


doParallel::registerDoParallel(cores = 30)

month_groups <- list(
  "1" = c(1, 2, 3, 4),
  "2" = c(5),
  "3" = c(6, 7),
  "4" = c(8, 9),
  "5" = c(10, 11, 12))

results <- list()
for (i in 1:5) {
  # pick random coords + compute means
  randn <- random_number(3, 1, 50)
  grass_rn <- randn[1]
  building_rn <- randn[2]
  trees_rn <- randn[3]
  # 2) for each image, compute EVI and cbind its val-column
  main_df <- NULL
  # 2) parallel inner loop: one full EVI df per image
  evi_chunks <- foreach(j = seq_along(image_list),
                        .packages = c("raster", "sf"),
                        .export      = c(
                          "rast_path","unmix_path",
                          "image_list","unmix_list","group","month_groups",
                          "grass_rn", "building_rn", "trees_rn",
                          "endmember_correction","EVI"
                        ),
                        .combine = "c") %dopar% {
                          img      <- image_list[j]
                          unmixImg <- unmix_list[j]
                          initial_name <- img
                          split_name <- str_split(initial_name,"_")
                          date <- split_name[[1]][3]
                          year <- strsplit(date, "-")[[1]][1]
                          
                          tree_val <- get_endmember_val(tree_shapefile, date, group, trees_rn, month_groups)
                          
                          if("Band" %in% colnames(tree_val))
                          {
                            tree_val <- tree_val
                          }else {
                            modified_date <- sub(paste0("^", year), "2018", date)
                            tree_val <- get_endmember_val(tree_shapefile, modified_date, group, trees_rn,month_groups)
                          }
                          grass_val<- get_endmember_val(grass_shapefile, date, group, grass_rn, month_groups)
                          # Check if it's FALSE
                          if("Band" %in% colnames(grass_val)){
                            grass_val <- grass_val
                          }else {
                            # Replace 2019 with 2018
                            modi_date <- sub(paste0("^", year), "2018", date)
                            grass_val <- get_endmember_val(grass_shapefile, modi_date, group, grass_rn, month_groups)
                          }
                          building_val <- get_endmember_val(building_shapefile, date, group, building_rn, month_groups)
                          # Check if it's FALSE
                          if ("Band" %in% colnames(building_val)){
                            building_val <- building_val
                          }else {
                            # Replace 2019 with 2018
                            mod_date <- sub(paste0("^", year), "2018", date)
                            building_val <- get_endmember_val(building_shapefile, mod_date, group, building_rn, month_groups)
                          }
                          ec  <- endmember_correction(rast_path, img, unmix_path, unmixImg, grass_val, building_val, tree_val)
                          evi <- EVI(ec)
                          df  <- na.omit(as.data.frame(evi, xy = TRUE))
                          print(dim(df))
                          name <- paste0("val_", tools::file_path_sans_ext(image_list[j]))
                          names(df)[3] <- name
                          list(df)   # return a single‐element list so 'c' will append it
                          
                        }
  main_df <- evi_chunks[[1]][, 1:3]
  
  # Add other images' value columns
  for (k in 2:length(evi_chunks)) {
    val_col <- evi_chunks[[k]][, 3, drop = FALSE]
    colname <- names(val_col)
    main_df <- cbind(main_df, val_col)
    names(main_df)[ncol(main_df)] <- colname
  }
  results[[i]] <- main_df
}
stopImplicitCluster()

# Apply smoothing to all data frames in parallel
smoothed_results <- future_lapply(results, smooth_one_df)


##################################### SMOOTHED DATA EXTRACTIONS#######################################################

# # 2) extract one series per replicate
# x <- 331935
# y <- 5811075

# x <- 331905
# y <- 5811055
smooth_df <- smoothed_results[[1]]
end_y <- length(smooth_df)
xy <- random_number(1, 1, end_y)

xy_number <- xy[1]

x <- smooth_df[["x"]][xy_number]
y <- smooth_df[["y"]][xy_number]

series_list <- lapply(smoothed_results, extract_series, x=x, y=y)

series_list_summer <- lapply(series_list, function(df) {
  # extract month as integer
  m <- as.integer(format(df$date, "%m"))
  y <- as.integer(format(df$date, "%y"))
  # keep only May(5)–Aug(8)
  df[m >= 1 & m <= 12,]
})

# 3) assemble a matrix of values (rows=dates, cols=reps)
vals_mat <- sapply(series_list_summer, `[[`, "value")
dates   <- series_list_summer[[1]]$date
sen_val <- get_sentinel_value(sentinel_path, x, y)
sen_evi <- sen_val$evi
summer_idx <- sen_val$idx


# 4) compute mean and sd
mean_vals <- rowMeans(vals_mat, na.rm=TRUE)
sd_vals   <- apply(vals_mat, 1, sd,      na.rm=TRUE)

# 5) build your summary df
summary_df <- data.frame(
  date  = dates,
  mean  = mean_vals,
  upper = mean_vals + sd_vals,
  lower = mean_vals - sd_vals
)
upper <- summary_df$upper
lower <- summary_df$lower

# red_flag <- vector("numeric", length = length(summary_df$date))
# for (i in 1:length(sen_evi)){
#   if (sen_evi[[i]] <= lower[[i]] && summer_idx[[i]] == TRUE)
#     red_flag[[i]] <- sen_evi[[i]]
# }
# red_flag

lower <- summary_df$lower
n <- length(sen_evi)
red_flag_full <- rep(NA, n)  # full-length output

i <- 1
while (i <= (n - 2)) {
  if (sen_evi[i] <= lower[i] && summer_idx[i] == TRUE &&
      sen_evi[i + 1] <= lower[i + 1] && summer_idx[i + 1] == TRUE &&
      sen_evi[i + 2] <= lower[i + 2] && summer_idx[i + 2] == TRUE) {
    
    red_flag_full[i]     <- sen_evi[i]
    red_flag_full[i + 1] <- sen_evi[i + 1]
    red_flag_full[i + 2] <- sen_evi[i + 2]
    
    i <- i + 3  # skip the sequence
  } else {
    i <- i + 1
  }
}

red_flag <- red_flag_full

#############################PLOT################################################

plot(summary_df$date, summary_df$mean,
     type  = "l",    lwd = 2,
     xaxt  = "n",    # suppress the default x‐axis
     xlab  = "Date",
     ylab  = "EVI",
     main  = "Expected EVI and actual EVI",
     ylim  = c(-0.1, max(summary_df$upper, sen_evi, na.rm = TRUE))  
)
# 2) add the shaded SD band
band_col <- adjustcolor("lightblue", alpha.f = 0.4)
with(summary_df, {
  polygon(
    x      = c(date, rev(date)),
    y      = c(upper, rev(lower)),
    col    = band_col,
    border = NA
  )
})

# 3) redraw the mean line on top
lines(summary_df$date, summary_df$mean, lwd = 2)
points(summary_df$date, sen_evi)
if (length(red_flag) >= 1){
  points(summary_df$date, red_flag, col="red", pch = 19)
}


# 4) draw a custom date axis at monthly intervals
axis.Date(
  side   = 1, 
  x       = summary_df$date,
  at      = seq(min(summary_df$date), max(summary_df$date), by = "month"),
  format  = "%b %Y",   # “Jan 2020”, “Feb 2020”, …
  cex.axis = 0.8       # shrink text if it’s crowded
)



