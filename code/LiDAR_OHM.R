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

#not producing a raster with only ground points 
#creating a digital terrain model (DTM) at 0.25m resolution from point cloud, las
#interpolates using nearest neighbor window (k) of 5 and inverse distance weighting power (p) of 0.5
#keeps the cells with the lowest values since we're classifying ground
#ground_rast<-grid_terrain(las, res = 0.25, algorithm = knnidw(k=5,p = 0.5), keep_lowest = TRUE)
#writeRaster(ground_rast,filename = file.path(pathtoGroundRasterOutput,"ground"), format="GTiff",overwrite=TRUE)

#subsetting las points to ground and veg (points already classified in file)
las_ground <- las[las$Classification == 2]
las_veg <- las[las$Classification == 1]

#convert to a raster
ground_rast <- grid_terrain(las_ground, res = 0.25, algorithm = knnidw(k=5,p = 0.5))
full_rast <- grid_terrain(las, res = 0.25, algorithm = knnidw(k=5, p=0.5))

# write raster from point subsets
writeRaster(ground_rast,filename = file.path(pathtoRasterOutputs,"ground"), format="GTiff",overwrite=TRUE)
writeRaster(full_rast,filename = file.path(pathtoRasterOutputs,"full"), format="GTiff",overwrite=TRUE)

chm <- full_rast - ground_rast



chm <- grid_canopy(las, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
plot(chm)



#==========================
las <- readLAS(pathtoPointCloud)
las_ground <- las[las$Classification == 2]
las_veg <- las[las$Classification == 1]

las_ground <- classify_ground(las = las_ground, algorithm = csf(), last_returns = T)
#plot(las_ground, color = "Classification")

dem = grid_terrain(las = las_ground,res = 0.1, algorithm = tin(), keep_lowest = T)
dsm = grid_canopy(las = las_veg, res = 0.1, algorithm = dsmtin())

chm <- dsm - dem