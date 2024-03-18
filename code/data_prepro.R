#create input data csv for model
#includes NDVI time series for Dec - Jul, height (LiDAR CHM), and distance from bank

library(terra)

pathtoCHMrast <- "./data/LiDAR/chm.tif"
pathtoDistRast <- "./data/TV_Banks/TV_Banks.tif"
pathtoNDVIstack <- "./data/NDVI_stack.tif"
pathtoNDVIstack <- "./data/PlanetScopeScenes/Jun23_Oct23/PSScene/20230602_171215_24_24c3_3B_AnalyticMS_SR_clip.tif"

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
