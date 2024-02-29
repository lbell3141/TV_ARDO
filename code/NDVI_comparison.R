#Divide point data into testing and training data groups
#there are three types of point data to consider: ARDO, non-ARDO veg, and bare ground

library(raster)
library(sp)
library(terra)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/ARDO")

pathtoARDOpts <- "./data/ARDO_pts/ARDO_pts_sf.shp"
pathtoVegpts <- "./data/veg_pts/non_ARDO_veg.shp"
pathtoGroundpts <- "./data/ground_pts/ground.shp"

#load in NDVI rasters and read as rasters
pathtoNDVIrasters <- "./data/2020_NDVI_rasters"
rast_files <- list.files(pathtoNDVIrasters, pattern = "NDVI_\\d{4}-\\d{2}-\\d{2}\\.tif$", full.names = T)
#extract raster names
raster_names <- list.files(pathtoNDVIrasters, pattern = "NDVI_\\d{4}-\\d{2}-\\d{2}\\.tif$", full.names = FALSE)
#load rasters with names
rast_list <- lapply(rast_files, function(file) {
  raster(file)
})
# assign raster names
names(rast_list) <- raster_names


#===filtering out partial rasters=======
#filter partial rasters out of the list of rasters by finding the majority extent
#pull extent values from each raster and place into a character list, rast_extents
rast_extents <- sapply(rast_list, function(r) paste(extent(r)))
#find the mode for the extent values by making a frequency table and pulling the most frequent value
extent_mode <- names(sort(table(rast_extents), decreasing = TRUE))[1]

#remove rasters with extents different from the mode found above
filtered_rasters <- rast_list[rast_extents == extent_mode]

#create raster stack now that all rasters have the same extent
NDVI_stack <- stack(filtered_rasters)
raster_names <- names(filtered_rasters)
raster_dates <- as.Date(gsub("NDVI_(\\d{4}-\\d{2}-\\d{2})\\.tif", "\\1", raster_names), format = "%Y-%m-%d")

NDVI_stack_raster <- rast(NDVI_stack)

#===pulling data per detection pixel===
#now using ARDO, veg, and ground shapefiles 
ARDO_pts <- vect(pathtoARDOpts)
veg_pts <- vect(pathtoVegpts)
ground_pts <- vect(pathtoGroundpts)

ARDO_NDVI <- extract(NDVI_stack_raster, ARDO_pts)
ARDO_NDVI <- ARDO_NDVI[, !names(ARDO_NDVI) %in% "ID"]
ARDO_NDVI <- as.matrix(ARDO_NDVI)

veg_NDVI <- extract(NDVI_stack_raster, veg_pts)
veg_NDVI <- veg_NDVI[, !names(veg_NDVI) %in% "ID"]
veg_NDVI <- as.matrix(veg_NDVI)

ground_NDVI <- extract(NDVI_stack_raster, ground_pts)
ground_NDVI <- ground_NDVI[, !names(ground_NDVI) %in% "ID"]
ground_NDVI <- as.matrix(ground_NDVI)

#store each pixel timeseries in new dataframe
create_dataframe <- function(row, dates) {
  df <- data.frame(Value = row, Date = dates)
}

#create list of dataframes
matrix_data <- ARDO_NDVI
list_of_ARDO_dfs <- lapply(1:nrow(matrix_data), function(i) {
  create_dataframe(matrix_data[i, ], raster_dates[i])
})
for (i in seq_along(list_of_ARDO_dfs)) {
  list_of_ARDO_dfs[[i]]$Date <- raster_dates
}

matrix_data <- veg_NDVI
list_of_veg_dfs <- lapply(1:nrow(matrix_data), function(i) {
  create_dataframe(matrix_data[i, ], raster_dates[i])
})
for (i in seq_along(list_of_dfs)) {
  list_of_veg_dfs[[i]]$Date <- raster_dates
}

matrix_data <- ground_NDVI
list_of_ground_dfs <- lapply(1:nrow(matrix_data), function(i) {
  create_dataframe(matrix_data[i, ], raster_dates[i])
})
for (i in seq_along(list_of_dfs)) {
  list_of_ground_dfs[[i]]$Date <- raster_dates
}


#===============PLOTTING ALL DATA TOGETHER======================================

#loop through each dataframe in the list to plot NDVI timeseries for each ARDO point
combined_plot <- ggplot() +
  # Plot ARDO NDVI
  lapply(seq_along(list_of_ARDO_dfs), function(i) {
    geom_line(data = list_of_ARDO_dfs[[i]], aes(x = Date, y = Value, color = "ARDO"))
  }) +
  # Plot vegetation NDVI
  lapply(seq_along(list_of_veg_dfs), function(i) {
    geom_line(data = list_of_veg_dfs[[i]], aes(x = Date, y = Value, color = "Vegetation"))
  }) +
  # Plot ground NDVI
  lapply(seq_along(list_of_ground_dfs), function(i) {
    geom_line(data = list_of_ground_dfs[[i]], aes(x = Date, y = Value, color = "Ground"))
  }) +
  labs(x = "Date", y = "NDVI", title = "Combined NDVI Time Series") +
  scale_color_manual(name = "Legend", 
                     values = c("ARDO" = "red", "Vegetation" = "blue", "Ground" = "wheat4"),
                     labels = c("ARDO", "Ground", "Vegetation")) +
  theme(legend.position = "bottom")

print(combined_plot)
 

