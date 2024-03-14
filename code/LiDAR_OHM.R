#process LiDAR point cloud to filter NDVI based on object height model
# LiDAR from Pima County (2021)

library("lidR")
library("rgdal")
library("RCSF")
library("raster")

pathtoPointCloud <- "./data/LiDAR/points.laz"
pathtoRasterOutputs <- "./data/LiDAR"
  
#read in point cloud
las <- readLAS(pathtoPointCloud)

#create new object with reduced number of points per square meter to decrease processing time
#this point cloud is only 22pts/m^2, so not entirely necessary
  #las_reduced<- decimate_points(las,random(10))
  #las <- las_reduced

#subsetting las points to ground and veg (points already classified in file)
las_ground <- las[las$Classification == 2]
las_veg <- las[las$Classification == 1]

#reclassifying ground points using Cloth Simulation Filtering (csf): inverts point cloud and classifies ground
las_ground <- classify_ground(las = las_ground, algorithm = csf(), last_returns = T)

#converting ground points into raster (Digital Elevation Model) using triangulation method (tin); keeping lowest points since we're classifying ground
dem = grid_terrain(las = las_ground,res = 0.1, algorithm = tin(), keep_lowest = T)
#converting veg points into raster (Digital Surface Model) using triangulation
dsm = grid_canopy(las = las_veg, res = 0.1, algorithm = dsmtin())
#the difference between the elevation of the ground and the elevation of the canopy will return a raster with the heights of the canopy
chm <- dsm - dem

# write raster from point subsets
writeRaster(chm,filename = file.path(pathtoRasterOutputs,"chm"), format="GTiff",overwrite=TRUE)
