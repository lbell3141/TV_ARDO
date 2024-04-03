#create train/test ARDO raster from polygon shapefile (exported from QGIS)

library(terra)

pathtoARDOPoly <- "./data/ARDOpolyhalf.shp"
pathtoReferenceRast <- "./data/LiDAR/chm.tif"
pathtoRastOutput <- "./data/model/TT_PA.tif"

ARDO_poly <- vect(pathtoARDOPoly)
ref_rast <- rast(pathtoReferenceRast)

poly_extent <- c(525508.5, 526288, 3566995, 3567224) 
cropped_rast <- crop(ref_rast, poly_extent)

poly_rast <- rasterize(ARDO_poly, cropped_rast)

writeRaster(poly_rast, pathtoRastOutput,overwrite=TRUE)
