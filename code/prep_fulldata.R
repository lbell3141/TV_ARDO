#simplify model to avoid overfitting
#adding full raster dataset instead of point values
#preproc to remove heights over 6m (stable overstory); remove NDVI under 1.9 (ground)

#model: present ~ height, distance from channel center, NDVI March 7
#model: present ~ 0.5 height, 0.5 distance from channel center, 0.5 NDVI April 12 2022

#preproc===============================================
library(terra)

pathtoCHMrast <- "./data/LiDAR/chm_filtered.tif"
pathtoDistRast <- "./data/TV_Banks/bed_dist.tif"
pathtoNDVI <- "./data/20_21_NDVI_stack.tif"

pathtoTTPARast <- "./data/model/TT_PA.tif"
pathtoCSVoutput <- "./data/model/rast_model/TT_data_rastmodel.csv"

height_rast <- rast(pathtoCHMrast)
dist_rast <- rast(pathtoDistRast)
NDVI_rast <- rast(pathtoNDVI)
  NDVI_rast[NDVI_rast < 0.18] <- NA
TTPA_rast <- rast(pathtoTTPARast)
#change raster resolution to all be the same
height_rast <- resample(height_rast, NDVI_rast)
dist_rast <- resample(dist_rast, NDVI_rast)
TTPA_rast <- resample(TTPA_rast, NDVI_rast)

#convert rasters to dataframes
heights_df <- as.data.frame(height_rast, xy = T)
dist_df <- as.data.frame(dist_rast, xy = T)
NDVI_df <- as.data.frame(NDVI_rast, xy = T)
TTPA_df <- as.data.frame(TTPA_rast, xy = T, na.rm = F)
  TTPA_df[is.na(TTPA_df)] <- 0

merged_df <- merge(heights_df, dist_df, by = c("x", "y"))
merged_df <- merge(merged_df, NDVI_df, by = c("x", "y"))
merged_df <- merge(merged_df, TTPA_df, by = c("x", "y"))

merged_df$OID <- seq_len(nrow(merged_df))

pres_col <- ncol(merged_df) - 1
OID_col <- ncol(merged_df)
end_of_NDVI_cols <- ncol(merged_df) - 2
merged_df <- merged_df[, c(OID_col,1, 2,pres_col,3:end_of_NDVI_cols)]

colnames(merged_df)[4] <- "present"
colnames(merged_df)[5] <- "height"
colnames(merged_df)[6] <- "dist_bank"
#colnames(merged_df)[7] <- "NDVI"


#add stipulations for height and ground NDVI
#height already filtered in OHM script
#merged_df <- merged_df[merged_df$NDVI >= 0.18, ]

write.csv(merged_df, pathtoCSVoutput, row.names = F)
