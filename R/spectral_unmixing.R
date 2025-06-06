library(raster)
library(RStoolbox)
library(raster)
library(ggfortify)
library(spacetime)
options(digits = 15)

# Define the function to get pixel values from a raster at specific coordinates
getPixelValues <- function(raster, x, y) {
  # Check if input is a raster stack or brick
  if (!is(raster, "RasterStack") && !is(raster, "RasterBrick")) {
    stop("Input must be a RasterStack or RasterBrick")
  }
  # Extract values
  values <- extract(raster, cbind(x, y))
  return(values)
}

summer2023 <- "M:\\spectral-unmixing\\brunschweig\\season\\2023-07-08.tif" 
summer2018 <- "M:\\spectral-unmixing\\brunschweig\\2018\\raster\\2018-07-24.tif"
my_raster <- stack(summer2023)

x_coords <-c(605884.35241388, 597015.27041402,  605844.62606236,  603774.28656804, 607434.03734735, 609344.77338910, 602124.81339392, 603264.66185505, 606095.39666017,
             604124.70977756, 605104.54067884,  606224.56587683,  604344.85966594, 603704.95771947, 605354.74183253, 601594.67715700, 603455.04413186, 605704.91914485,
             603934.93829377, 607184.99571110,  601544.81117353,  605774.21984302, 602834.89291018, 608434.99654031, 603804.82767571, 601997.45176002, 605844.00057853,
             601343.96007151, 602025.02029399,  600955.18869249,  601385.30020858, 603444.34389349, 607914.89023268, 602024.71019722, 604325.42330900, 605524.06270606,
             603995.13082208, 604524.86683747,  600944.80810616,  601545.37358462, 605214.61848969, 606084.86966286, 603794.85026818, 605335.55162396, 604304.38362382,
             605774.28906632, 600125.20448450,  605564.81619286,  602014.48674017, 604488.87077051, 601514.94141965, 603804.47109654, 605254.88785562, 600594.57098086,
             602894.57743594, 603505.16630111,  607614.96905017,  602155.86598629, 603484.57035356, 603714.52026222, 603914.85641389, 605884.68866015, 604153.87142431,
             604095.42293653, 604504.37753244,  608805.08644681,  602154.98347404, 605374.65080647, 604214.75961462, 603944.95083433, 603504.38658923, 605572.47616210,
             605555.11174017, 604003.72974938,  607494.50647028,  605994.50201132, 600815.13073052, 602414.73516515, 606824.74821216, 605514.67297378, 607154.29515967,
             605815.46074196, 605914.42621424,  608694.83478299,  605894.71524881, 607724.98193534, 604124.55308533, 607205.29890495, 603644.73790031, 605223.53060541,
             603234.98268027, 607515.77780468,  608164.62454239,  607635.15820545, 603284.83276058, 603904.56008771)

y_coords <-c(5794904.90526522, 5784795.27742394, 5784395.27011304, 5794415.18098868, 5787455.68545425, 5791535.12023019, 5796505.58514599, 5797895.43989543, 5791905.90097722,
             5795713.66478177, 5784703.75372495, 5784615.86404430, 5796745.92421278, 5790735.03360869, 5785105.35503072, 5795296.35366749, 5790545.91561108, 5794325.09599137,
             5795955.07904738, 5787075.42469928, 5786475.51631602, 5788845.13564746, 5790415.13268498, 5792294.81865189, 5787834.97731442, 5793005.55580780, 5791785.56497453,
             5790825.02979738, 5793385.96403187, 5785845.49756323, 5790805.24061383, 5791265.24517007, 5787226.65458200, 5796195.05347378, 5787544.49772795, 5792296.23977339,
             5796835.99326819, 5785945.83888284, 5786545.31993459, 5795256.02931317, 5789981.95943597, 5792815.55087392, 5787845.15047676, 5789235.00819428, 5792235.62237172,
             5788985.15105885, 5795175.63878127, 5789505.26258405, 5795745.40096096, 5791295.43343149, 5786514.68887557, 5790115.15755363, 5788334.93847815, 5793575.70545686,
             5787504.87047453, 5790495.12300083, 5790685.59478960, 5795375.91999403, 5791495.97073622, 5784015.70694278, 5794315.80285308, 5788675.15152941, 5795914.82326430,
             5795665.41734951, 5794055.48500819, 5791034.81997643, 5795385.82133734, 5797145.52899016, 5792405.51507242, 5795955.28158522, 5791275.36443168, 5785315.02719011,
             5787795.44100119, 5791526.05553910, 5787925.37410835, 5791765.25067044, 5796256.04318109, 5794505.97117331, 5792026.16675357, 5798535.14786945, 5798484.57149977,
             5792105.32348778, 5798215.41734952, 5791104.85657574, 5794885.52964838, 5793786.42077621, 5790065.30746914, 5796085.88442340, 5791655.35340665, 5792085.63900867,
             5793635.35365762, 5791984.93044173, 5787445.06533302, 5794745.99085592, 5792825.13227703, 5790375.32117592)

# Class labels for each coordinate pair
classes <- c("grass", "buildings", "trees")

# Pre-allocate a matrix to hold the results
num_bands <- nlayers(my_raster)
result_matrix <- matrix(NA, nrow = length(x_coords), ncol = num_bands)

# Populate the matrix with pixel values
for (i in 1:length(x_coords)) {
  # Get pixel values at specified coordinates
  pixel_values <- getPixelValues(my_raster, x_coords[i], y_coords[i])
  
  # Store the values in the corresponding row of the matrix
  result_matrix[i, ] <- pixel_values
}

# Convert the result matrix to a dataframe
result_df <- as.data.frame(result_matrix)

# Naming the columns as Band1, Band2, ..., BandN
names(result_df) <- paste0("Band", 1:num_bands)

result_df$class <- classes
result_df
grass <- subset(result_df, class == "grass")
row.names(grass) <- NULL
grass[1, 2]
building <- subset(result_df, class == "buildings")
row.names(building) <- NULL
building[1, 1]
trees <- subset(result_df, class == "trees")
row.names(trees) <- NULL

result_list <- list()
member_df <- list(grass = grass, building = building, trees = trees)

for (end_member_name in names(member_df)){
  end_member <- member_df[[end_member_name]]
  # Loop through each row
  for (i in 2:ncol(end_member) - 1){
    # Loop through each column
    for (j in 2:nrow(end_member) - 1){
      print(paste0("col", i,"row", j))
      x <- 1
      y <- 0
      while (x > 0){
        if (x == 1){
          # Perform multiplication between rt and rb, and between rt and rg
          result <- end_member[j, i] 
          split_sn <- strsplit(end_member_name, "")[[1]]
          
          
          # Create a name for the result and store in the list
          result_list[[paste0("r", split_sn[1], x, "_", j, "_b", i)]] <- result
        }
        x <- x - 0.1
        y <- y + 0.1
        if (x <= 0.6){
          break 
        }
        if (end_member_name == "grass"){
          # Perform multiplication between rt and rb, and between rt and rg
          result_g_b <- grass[j, i] * x + building[j, i] * y
          # print(paste0("grass", grass[j, i], "x", x, "building", building[j, i], "y", y))
          result_g_t <- grass[j, i] * x + trees[j, i] * y
          # Create a name for the result and store in the list
          result_list[[paste0("rg", x,"_", j, "b", "_b", i)]] <- result_g_b
          result_list[[paste0("rg", x,"_", j, "t", "_b", i)]] <- result_g_t
        }
        else if (end_member_name == "building"){
          # Perform multiplication between rt and rb, and between rt and rg
          result_b_g <- building[j, i] * x + grass[j, i] * y
          result_b_t <- building[j, i] * x + trees[j, i] * y
          # Create a name for the result and store in the list
          result_list[[paste0("rb", x,"_", j, "g", "_b", i)]] <- result_b_g
          result_list[[paste0("rb", x,"_", j, "t", "_b", i)]] <- result_b_t
        } 
        else{
          result_t_g <- trees[j, i] * x + grass[j, i] * y
          result_t_b <- trees[j, i] * x + building[j, i] * y
          # Create a name for the result and store in the list
          result_list[[paste0("rt", x,"_", j, "g", "_b", i)]] <- result_t_g
          result_list[[paste0("rt", x,"_", j, "b", "_b", i)]] <- result_t_b
        }
      }
    }
  }
}

# Initialize empty DataFrames for each subclass
data_g <- data.frame()
data_b <- data.frame()
data_t <- data.frame()

mixture_number <- c(1.0, 0.9, 0.8, 0.7, 0.6)
names_classes <- c("g", "b", "t")
bands <- 1:10  # Assuming 10 spectral bands

for (fraction in mixture_number) {
  for (n in names_classes) {
    if (fraction == 1.0) {
      # Handle pure pixels
      fraction_str <- ifelse(fraction == 1.0, "1", as.character(fraction))
      # Loop over all samples (assuming 31 samples)
      for (j in 1:31) {
        sample_data <- data.frame()
        for (b in bands) {
          nam <- paste0("r", n, fraction_str, "_", j, "_b", b)
          value <- result_list[[nam]]
          if (!is.null(value)) {
            sample_data[1, paste0("Band", b)] <- value
          } else {
            sample_data[1, paste0("Band", b)] <- NA
          }
        }
        # Add additional information
        sample_data$MixtureFraction <- fraction
        sample_data$SubClass <- n
        sample_data$Class <- n
        sample_data$SampleID <- j  # Assign SampleID
        # Append to the main data frame
        if (n == "g") {
          data_g <- rbind(data_g, sample_data)
        } else if (n == "b") {
          data_b <- rbind(data_b, sample_data)
        } else if (n == "t") {
          data_t <- rbind(data_t, sample_data)
        }
      }
    } else {
      # Handle mixture pixels
      sub_names <- names_classes[names_classes != n]
      for (s in sub_names) {
        for (i in 1:31) {
          sample_data <- data.frame()
          missing_data <- FALSE
          
          # Collect spectral data across all bands
          for (b in bands) {
            fraction_str <- as.character(fraction)
            nam <- paste0("r", n, fraction_str, "_", i, s, "_b", b)
            
            if (!nam %in% names(result_list)) {
              missing_data <- TRUE
              print(paste("Key not found:", nam))
              break  # Skip this sample if data is missing
            }
            
            value <- result_list[[nam]]
            sample_data[1, paste0("Band", b)] <- value
          }
          
          if (missing_data) {
            next  # Skip to the next sample if data is incomplete
          }
          
          # Add additional information to the sample
          sample_data$MixtureFraction <- fraction
          sample_data$SubClass <- s
          sample_data$Class <- n
          sample_data$SampleID <- i
          
          # Append the sample to the appropriate DataFrame
          if (n == "g") {
            data_g <- rbind(data_g, sample_data)
          } else if (n == "b") {
            data_b <- rbind(data_b, sample_data)
          } else if (n == "t") {
            data_t <- rbind(data_t, sample_data)
          }
        }
      }
    }
  }
}


# Load necessary libraries
library(randomForest)
library(raster)

# Define feature columns
feature_columns <- paste0("Band", 1:10)

# Function to train and evaluate a Random Forest model
train_and_evaluate_rf_model <- function(data, feature_columns, target_column = "MixtureFraction") {
  # Ensure target variable is numeric
  data[[target_column]] <- as.numeric(data[[target_column]])
  
  # Split data into training and testing sets
  set.seed(123)  # For reproducibility
  train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Train the Random Forest model
  rf_model <- randomForest(
    x = train_data[, feature_columns],
    y = train_data[[target_column]],
    ntree = 100,
    importance = TRUE
  )
  
  # Evaluate the model on test data
  predictions <- predict(rf_model, test_data[, feature_columns])
  actual <- test_data[[target_column]]
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean((predictions - actual)^2)
  cat("Model MSE:", mse, "\n")
  
  # Return the trained model
  return(rf_model)
}

# Function to predict mixture fractions using the Random Forest model
predict_mixture_fraction <- function(rf_model, sentinel_image, feature_columns) {
  # Ensure band names match the model features
  names(sentinel_image) <- feature_columns
  
  # Get the number of rows and columns from the Sentinel-2 image
  rows <- nrow(sentinel_image)
  cols <- ncol(sentinel_image)
  
  # Create an empty raster with the same dimensions and properties as the input image
  results <- raster(nrows = rows, ncols = cols, crs = crs(sentinel_image))
  extent(results) <- extent(sentinel_image)
  results <- setValues(results, NA)
  
  # Loop through each pixel in the Sentinel-2 data
  for (row in 1:rows) {
    for (col in 1:cols) {
      # Extract band values for the current pixel
      pixel_values <- as.numeric(sentinel_image[row, col])
      
      # Check if any band value is NA
      if (any(is.na(pixel_values))) {
        # Assign NA to the results raster
        results[row, col] <- NA
      } else {
        # Create a data frame with the same structure as the training data
        pixel_df <- as.data.frame(t(pixel_values))
        colnames(pixel_df) <- feature_columns
        
        # Predict the fraction using the Random Forest model
        predicted_value <- predict(rf_model, pixel_df)
        
        # Assign the predicted value to the results raster
        results[row, col] <- predicted_value
      }
    }
  }
  
  return(results)
}

# Load the Sentinel-2 image
sentinel_image <- stack("M:\\spectral-unmixing\\brunschweig\\test_algo\\grass.tif")
names(sentinel_image) <- feature_columns

# Grass
cat("Processing Grass\n")
rf_model_g <- train_and_evaluate_rf_model(data_g, feature_columns)
predicted_mixture_g <- predict_mixture_fraction(rf_model_g, sentinel_image, feature_columns)
predicted_mixture_g

# Buildings
cat("Processing Buildings\n")
rf_model_b <- train_and_evaluate_rf_model(data_b, feature_columns)
predicted_mixture_b <- predict_mixture_fraction(rf_model_b, sentinel_image, feature_columns)

# Trees
cat("Processing Trees\n")
rf_model_t <- train_and_evaluate_rf_model(data_t, feature_columns)
predicted_mixture_t <- predict_mixture_fraction(rf_model_t, sentinel_image, feature_columns)

# Stack the predicted images
predicted_stack <- stack(predicted_mixture_g, predicted_mixture_b, predicted_mixture_t)
names(predicted_stack) <- c("GrassFraction", "BuildingFraction", "TreeFraction")

# Save the stacked image
writeRaster(
  predicted_stack,
  filename = "M:\\spectral-unmixing\\brunschweig\\rf\\results\\predicted_mixture_stack3.tif",
  format = "GTiff",
  overwrite = TRUE
)
