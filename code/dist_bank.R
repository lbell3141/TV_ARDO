#calculating distance from bank paramter 
#calculated for the position of each cell, so is constant for each year
#shapefile includes two lines running along the north most and south most edges of the river

library(terra)
library(sf)

pathtoBankShapefile <- "./data/TV_Banks/TV_Banks.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/TV_Banks"

#load in riverbank lines and raster with reference crs and positions
banks <- vect(pathtoBankShapefile)
ref_rast <- rast(pathtoReferenceRast)

#create empty rast to store distance values in
dist_rast <- rast(ref_rast)

#calc distance from nearest bank line
distance_values <- distance(banks)

#place distance values in empty distance raster
values(dist_rast) <- distance_values

#save output distance raster
writeRaster(dist_rast, filename = file.path(pathtoRastOutputs,"bank_dist"), format="GTiff",overwrite=TRUE)