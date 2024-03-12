#process LiDAR point cloud to filter NDVI based on object height model
# LiDAR from Pima County (2021)

library("lidR")
library("rgdal")
library("RCSF")
library("raster")

pathtoPointCloud <- "./data/LiDAR/points.laz"
pathtoGroundRasterOutput <- "./data/LiDAR"

#read in point cloud
las <- readLAS(pathtoPointCloud)

#create new object with reduced number of points per square meter to decrease processing time
  #las_reduced<- decimate_points(las,random(10))
  #las <- las_reduced

#creating a digital terrain model (DTM) at 0.25m resolution from point cloud las
#interpolates using nearest neighbor window (k) of 5 and inverse distance weighting power (p) of 0.5
#keeps the cells with the lowest values since we're classifying ground
ground_rast<-grid_terrain(las, res = 0.25, algorithm = knnidw(k=5,p = 0.5), keep_lowest = TRUE)
writeRaster(ground_rast,filename = file.path(pathtoGroundRasterOutput,"ground"), format="GTiff",overwrite=TRUE)

chm <- grid_canopy(las, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
plot(chm)
