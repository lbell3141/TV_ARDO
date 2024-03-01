#Computing summary stats from each group (ARDO, veg, ground) and plotting
#lines 3-85 from NDVI_comparison to get list of dataframes loaded into environment

library(raster)
library(sp)
library(terra)
library(dplyr)
library(ggplot2)

setwd("C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/TV_ARDO")

pathtoARDOpts <- "./data/object_points/ARDO_pts/ARDO_pts_sf.shp"
pathtoVegpts <- "./data/object_points/veg_pts/non_ARDO_veg.shp"
pathtoGroundpts <- "./data/object_points/ground_pts/ground.shp"
pathtoNDVIrasters <- "./data/22_23_NDVI_rasters"

#load in NDVI rasters and read as rasters
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
for (i in seq_along(list_of_veg_dfs)) {
  list_of_veg_dfs[[i]]$Date <- raster_dates
}

matrix_data <- ground_NDVI
list_of_ground_dfs <- lapply(1:nrow(matrix_data), function(i) {
  create_dataframe(matrix_data[i, ], raster_dates[i])
})
for (i in seq_along(list_of_ground_dfs)) {
  list_of_ground_dfs[[i]]$Date <- raster_dates
}

#vertically combining dfs (don't merge by date- different lengths when combinig groups)
ARDO_combined <- list_of_ARDO_dfs[[1]]
for (i in 2:length(list_of_ARDO_dfs)) {
  ARDO_combined <- rbind(ARDO_combined, list_of_ARDO_dfs[[i]])
}
ARDO_combined$Group <- "ARDO"

veg_combined <- list_of_veg_dfs[[1]]
for (i in 2:length(list_of_veg_dfs)) {
  veg_combined <- rbind(veg_combined, list_of_veg_dfs[[i]])
}
veg_combined$Group <- "veg"

ground_combined <- list_of_ground_dfs[[1]]
for (i in 2:length(list_of_ground_dfs)) {
  ground_combined <- rbind(ground_combined, list_of_ground_dfs[[i]])
}
ground_combined$Group <- "ground"

#vertically combine each of the three dataframes into one large df
full_combine <- rbind(ARDO_combined, veg_combined, ground_combined)

#group by group and date to then calculate summary stats (min, max, mean, median, stdev)
#store results in new df: 
summary_df <- full_combine %>%
  group_by(Group, Date) %>%
  summarize(
    Min = min(Value, na.rm = T),
    Max = max(Value, na.rm = T),
    Median = median(Value, na.rm = T),
    Mean = mean(Value, na.rm = T),
    StDev = sd(Value, na.rm = T)) 

#reformatting df so it's easier to compare values by date
#not necessary for any subsequent code
#reformatted_df <- summary_df %>%
#  arrange(Date)

# Plot mean values by Date for each Group with shaded regions representing standard deviation
group_col <- c("ARDO" = "red", "veg" = "blue", "ground" = "tan")
ggplot(summary_df, aes(x = Date, y = Mean, color = Group, fill = Group)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = Mean - StDev, ymax = Mean + StDev, fill = Group), color = NA, alpha = 0.1) +
  labs(title = "Average NDVI (May 2022-2023)", x = "Date", y = "Mean NDVI", color = "Group") +
  scale_color_manual(values = group_col) +
  scale_fill_manual(values = group_col) +
  theme_minimal()




