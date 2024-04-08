#merge Pima LiDAR point clouds from AOIs around the Tanque Verde into a continuous cloud
library(lidR)
library(sf)

pathtoLiDARSegments <- "./data/LiDAR/LiDAR_segments"
pathto60mAOI <- "./data/TV_AOI/TV_buff_60m.shp"

TV_laz_files <- list.files(pathtoLiDARSegments, pattern = "\\.laz$", full.names = TRUE)
TV_AOI <- st_read(pathto60mAOI)

TV_clouds <- lapply(TV_laz_files, readLAS)
merged <- merge_spatial(TV_clouds, TV_AOI)

