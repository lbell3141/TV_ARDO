#create a plot showing removed ARDO ploygons within Tanque Verde AOI
#ARDO polygons manually created in and exported from google earth pro 
#determining removal based on changes in average July NDVI from 2021 to 2023 for each polygon
#NDVI data calculated from Plant labs PSS product
#2021 raster stack made in NDVI_comparison.R

library(sf)
library(terra)

pathtoKMLfile <-  "./Untitled Polygon.kml"
pathto2021rasts <- "./data/20_21_NDVI_stack.tif"
pathto2023rasts <- "./data/22_23_NDVI_stack.tif"


ARDO_kml <- st_read(pathtoKMLfile)
ARDO_kml <- ARDO_kml[1]

stack_21 <- rast(pathto2021rasts)
stack_23 <- rast(pathto2023rasts)
