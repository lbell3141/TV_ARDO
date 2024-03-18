#calculating distance from bank paramter 
#calculated for the position of each cell, so is constant for each year
#shapefile includes two lines running along the north most and south most edges of the river

library(terra)
library(sf)

pathtoBankShapefile <- "./data/TV_Banks/TV_Banks.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/TV_Banks"

centerline <- vect(pathtoBankShapefile)
ref_rast <- rast(pathtoReferenceRast)

dist_rast <- distance(ref_rast, centerline)


