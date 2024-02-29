#create shapefiles for areas of known ARDO, trees, understory, bareground, and water
#make raster stack of NDVI
#mask stack with shapefiles
#find average NDVI values for each raster in each masked stack
#plot NDVI time series per masked stack
#plot all NDVI time series together
#identify areas of unique NDVI for ARDO


library(raster)
library(sp)
library(ggplot2)

#load in 2021 ARDO coords from csv
pathto2021ARDO <- "./data/Arundo_donax_stand_survey_2021/GPSCompiledWaypointsArundoMay72021_ExcelToTable1_XYTableToPoint_2.csv"
ARDO_detections <- read.csv(pathto2021ARDO, header = T, sep = ",")

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


#===pulling data per detection pixel===
#convert lat long columns in csv to usable coordinates
ARDO_points <- SpatialPoints(coords = ARDO_detections[, c("lon", "lat")])

#convert to crs of filtered_rasters
#I'm assigning (WGS84, EPSG code 4326) to detection points bc I can't find the CRS anywhere
proj4string(ARDO_points) <- CRS("+proj=longlat +datum=WGS84")
#do transformation
ARDO_points <- spTransform(ARDO_points, CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"))


#pull ndvi values for each ARDO pt from each layer in the stack
ARDO_NDVI <- extract(NDVI_stack, ARDO_points)



#store in new dataframe
raster_dates <- as.Date(gsub("NDVI_(\\d{4}-\\d{2}-\\d{2})\\.tif", "\\1", raster_names), format = "%Y-%m-%d")
create_dataframe <- function(row, dates) {
  df <- data.frame(Value = row, Date = dates)
}

#create list of dataframes
matrix_data <- ARDO_NDVI
list_of_dfs <- lapply(1:nrow(matrix_data), function(i) {
  create_dataframe(matrix_data[i, ], raster_dates[i])
})



#====plot ARDO NDVI============
#make plot aesthetics
combined_plot <- ggplot() +
  labs(x = "Date", y = "NDVI", title = "2020 Detected ARDO NDVI")+
  theme(legend.position = "none")

#manually add date values to each date column in the list
for (i in seq_along(list_of_dfs)) {
  list_of_dfs[[i]]$Date <- raster_dates
}

#loop through each dataframe in the list to plot NDVI timeseries for each ARDO point
for (i in seq_along(list_of_dfs)) {
  combined_plot <- combined_plot +
    geom_line(data = list_of_dfs[[i]], aes(x = Date, y = Value))
  print(combined_plot)
}



#================= average value calculation and plotting
# Calculate average value per column in the matrix
average_values <- colMeans(matrix_data, na.rm = T)

# Create a dataframe with average value and date
average_df <- data.frame(Average_Value = average_values, Date = raster_dates)

#plot
combined_and_avg_plot <- combined_plot +
  geom_line(data = average_df, aes(x = Date, y = Average_Value), color = "red", linewidth = 0.8)
print(combined_and_avg_plot)


