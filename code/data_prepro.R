#create input data csv for model
#includes NDVI time series for Dec - Jul, height (LiDAR CHM), and distance from bank

library(terra)

pathtoCHMrast <- "./data/LiDAR/chm.tif"
pathtoDistRast <- "./data/TV_Banks/bed_dist.tif"
pathtoNDVIstack <- "./data/20_21_NDVI_stack.tif"

pathtoARDOpts <- "./data/object_points/ARDO_pts/GE_ARDO_rept.shp"
pathtoVegpts <- "./data/object_points/veg_pts/test_canopy.shp"
pathtoGroundpts <- "./data/object_points/ground_pts/gr_pts.shp"
pathtoCSVoutput <- "./data/model/train_test_data.csv"

height_rast <- rast(pathtoCHMrast)
dist_rast <- rast(pathtoDistRast)
NDVI_rast_stack <- rast(pathtoNDVIstack)

#change raster resolution to all be the same
height_rast <- resample(height_rast, NDVI_rast_stack)
dist_rast <- resample(dist_rast, NDVI_rast_stack)

#convert rasters to dataframes
heights_df <- as.data.frame(height_rast, xy = T)
dist_df <- as.data.frame(dist_rast, xy = T)
NDVI_df <- as.data.frame(NDVI_rast_stack, xy = T)

#merge data frames by xy positions (need to have same CRS!)
merged_df <- merge(heights_df, dist_df, by = c("x", "y"))
merged_df <- merge(merged_df, NDVI_df, by = c("x", "y"))

merged_df$present <- NA
merged_df$OID <- seq_len(nrow(merged_df))
OID_col <- ncol(merged_df)
pres_col <- ncol(merged_df) - 1
end_of_NDVI_cols <- ncol(merged_df) - 2
merged_df <- merged_df[, c(OID_col,1, 2,pres_col,3:end_of_NDVI_cols)]

colnames(merged_df)[5] <- "height"
colnames(merged_df)[6] <- "dist_bank"

merged_df <- replace(merged_df, is.na(merged_df), -9999)

write.csv(merged_df, pathtoCSVoutput, row.names = F)


#===============================================================================
#extract training/testing points and build dataframe

ARDO_pts <- vect(pathtoARDOpts)
veg_pts <- vect(pathtoVegpts)
ground_pts <- vect(pathtoGroundpts)

full_stack <- rast(list(height_rast, dist_rast, NDVI_rast_stack))

ARDO_stack_vals <- extract(full_stack, ARDO_pts)
ARDO_stack_vals$Group <- "ARDO"
ARDO_stack_vals$present <- 1
Veg_stack_vals <- extract(full_stack, veg_pts)
Veg_stack_vals$Group <- "Veg"
Veg_stack_vals$present <- 0
Ground_stack_vals <- extract(full_stack, ground_pts)
Ground_stack_vals$Group <- "Gr"
Ground_stack_vals$present <- 0

combined_df <- rbind(ARDO_stack_vals, Veg_stack_vals, Ground_stack_vals)
combined_df$OID <- seq_len(nrow(combined_df))

OID_col <- ncol(combined_df)
pres_col <- ncol(combined_df) - 1
Group_col <- ncol(combined_df) - 2
end_of_NDVI_cols <- ncol(combined_df) - 3
combined_df <- combined_df[, c(OID_col,1, Group_col,pres_col,2,3:end_of_NDVI_cols)]

colnames(combined_df)[5] <- "height"
colnames(combined_df)[6] <- "dist_bank"

combined_df <- replace(combined_df, is.na(combined_df), -9999)

write.csv(combined_df, pathtoCSVoutput, row.names = F)
