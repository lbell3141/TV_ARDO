#calculating distance from bank parameter 
#calculated for the position of each cell, so is constant for each year
#shapefile includes two lines running along the north most and south most edges of the river

library(terra)
library(sf)
library(rgeos)
library(raster)

pathtoBankShapefile <- "./data/TV_Banks/TV_Banks.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/TV_Banks/bank_dist.tif"

<<<<<<< HEAD
#load in riverbank lines and raster with reference crs and positions
banks <- shapefile(pathtoBankShapefile)
=======
centerline <- vect(pathtoBankShapefile)
>>>>>>> 4bad8488a8c65f08a5505117ba9fb0c599b2dda9
ref_rast <- rast(pathtoReferenceRast)

dist_rast <- distance(ref_rast, centerline)

<<<<<<< HEAD
dd <- gDistance(banks, as(ref_rast, "SpatialPoints"), byid = TRUE)



#calc distance from nearest bank line
distance_values <- distance(banks)

#place distance values in empty distance raster
values(dist_rast) <- distance_values

#save output distance raster
writeRaster(dist_rast, filename = file.path(pathtoRastOutputs,"bank_dist"), format="GTiff",overwrite=TRUE)






#test


library(terra)
library(sf)
library(rgeos)
library(rgdal)

pathtoBankShapefile <- "./data/TV_Banks/TV_Banks.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/TV_Banks"

#load in riverbank lines and raster with reference crs and positions
banks <- readOGR(pathtoBankShapefile)

ref_rast <- rast(pathtoReferenceRast)

#create empty rast to store distance values in
dist_rast <- rast(ref_rast)

dd <- gDistance(banks, as(ref_rast, "SpatialPoints"), byid = TRUE)


spatial_lines <- SpatialLines(list(Lines(list(Line(coordinates(shapefile))), ID = "1")))
=======
writeRaster(dist_rast,pathtoRastOutput,overwrite=TRUE)
>>>>>>> 4bad8488a8c65f08a5505117ba9fb0c599b2dda9
