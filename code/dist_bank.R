#calculating distance from bank parameter 
#calculated for the position of each cell, so is constant for each year
#shapefile includes two lines running along the north most and south most edges of the river

library(terra)
library(raster)

pathtoBankShapefile <- "./data/TV_Banks/TV_bank_poly.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/TV_Banks/bed_dist.tif"

#load in riverbank lines and raster with reference crs and positions
banks <- vect(pathtoBankShapefile)

ref_rast <- rast(pathtoReferenceRast)

bank_raster <- rasterize(banks, ref_rast)

dist_rast <- distance(bank_raster)
plot(dist_rast)

#save output distance raster
writeRaster(dist_rast, pathtoRastOutput,overwrite=TRUE)

