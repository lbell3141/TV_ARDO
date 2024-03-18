#create input data csv for model
#includes NDVI time series for Dec - Jul, height (LiDAR CHM), and distance from bank

library(terra)

pathtoCHMrast <- "./data/LiDAR/chm.tif"
pathtoDistRast <- "./data/TV_Banks/TV_Banks.tif"
pathtoNDVIstack <- "./data/NDVI_stack.tif"
pathtoNDVIstack <- "./data/PlanetScopeScenes/Jun23_Oct23/PSScene/20230602_171215_24_24c3_3B_AnalyticMS_SR_clip.tif"

pathtoARDOpts <- "./data/object_points/ARDO_pts/ARDO_pts.shp"
pathtoOtherpts <- "./data/object_points/Other_pts/otherpts.shp"
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

#===============================================================================
#extract training/testing points and build dataframe

ARDOpts <- vect(pathtoARDOpts)
Otherpts <- vect(pathtoOtherpts)

full_stack <- rast(list(height_rast, dist_rast, NDVI_rast_stack))

ARDO_stack_vals <- extract(full_stack, ARDOpts)
ARDO_stack_vals$Group <- "ARDO"
ARDO_stack_vals$present <- 1
Other_stack_vals <- extract(full_stack, Otherpts)
Other_stack_vals$Group <- "Other"
Other_stack_vals$present <- 0

combined_df <- rbind(ARDO_stack_vals, Other_stack_vals)
combined_df$OID <- seq_len(nrow(combined_df))

end_of_NDVI_cols <- ncol(combined_df) - 3
combined_df <- combined_df[, c(10,1,8,9,2,3:end_of_NDVI_cols)]

write.csv(combined_df, pathtoCSVoutput, row.names = F)
