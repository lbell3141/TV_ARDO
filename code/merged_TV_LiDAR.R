#merge Pima LiDAR point clouds from AOIs around the Tanque Verde into a continuous cloud
library(lidR)
library(sf)

pathtoLiDARSegments <- "./data/LiDAR/LiDAR_segments"
pathto60mAOI <- "./data/TV_AOI/TV_buff_60m.shp"

TV_laz_files <- list.files(pathtoLiDARSegments, pattern = "\\.laz$", full.names = TRUE)
TV_AOI <- st_read(pathto60mAOI)
TV_clouds <- list()

for (file in TV_laz_files) {
  cloud <- readLAS(file)
  cloud_clipped <- lasclip(cloud, TV_AOI)
  TV_clouds[[length(TV_clouds) + 1]] <- cloud_clipped
}

# Combine all the clipped point clouds into a single LAS object
merged <- do.call(rbind, TV_clouds)





TV_catalog <- catalog(TV_laz_files)

# Clip the LiDAR point clouds to the AOI boundaries
TV_clouds <- clip_roi(TV_catalog, TV_AOI)

# Merge all the clipped point clouds into a single continuous cloud
merged <- merge(TV_clouds)
