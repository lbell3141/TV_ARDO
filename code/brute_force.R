#brute force ARDO ID with LiDAR exclusion

library("lidR")
library("rgdal")
library("RCSF")
library("raster")
library("terra")

pathtoPointCloud <- "./data/LiDAR/points.laz"
pathtoRasterOutputs <- "./data/LiDAR"
pathto60mAOI <- "./data/TV_AOI/TV_buff_60m.shp"
pathtoNDVIrast <- "./data/Dec20_Jul21_NDVI_rasters/NDVI_2021-05-20.tif"
pathtoPSS <- "./data/PlanetScopeScenes/dec20_Jul21/PSScene/20210414_172809_11_2251_3B_AnalyticMS_SR_clip.tif"
pathtoPSS2 <- "./data/PlanetScopeScenes/dec20_Jul21/PSScene/20201227_174508_1011_3B_AnalyticMS_SR_clip.tif"

#===============================================================================
#================processing LiDAR and filtering derived raster==================
#===============================================================================

#read in point cloud
las <- readLAS(pathtoPointCloud)

#subsetting las points to ground and veg (points already classified in file)
las_ground <- las[las$Classification == 2]
las_veg <- las[las$Classification == 1]

#reclassifying ground points using Cloth Simulation Filtering (csf): inverts point cloud and classifies ground
las_ground <- classify_ground(las = las_ground, algorithm = csf(), last_returns = T)

#converting ground points into raster (Digital Elevation Model) using triangulation method (tin); keeping lowest points since we're classifying ground
dem = grid_terrain(las = las_ground,res = 0.05, algorithm = tin(), keep_lowest = T)
#dem_3m = grid_terrain(las = las_ground,res = 3, algorithm = tin(), keep_lowest = T)
#converting veg points into raster (Digital Surface Model) using triangulation
dsm = grid_canopy(las = las_veg, res = 0.05, algorithm = dsmtin())
#dsm_3m = grid_canopy(las = las_veg, res = 3, algorithm = dsmtin())

#the difference between the elevation of the ground and the elevation of the canopy will return a raster with the heights of the canopy
chm <- dsm - dem
#chm_3m <- dsm_3m - dem_3m
#chm_3m <- aggregate(chm, 30, 'max')
chm_3norm <- aggregate(chm, 30)

tnorm <- chm
tnorm[tnorm <= 1.5 | tnorm >= 6.5] <- NA
tnorm[tnorm <= 0.5 | tnorm >= 6.5] <- NA

plot(tnorm)

#mask raster to AOI
AOI_60m <- shapefile(pathto60mAOI)
mask <- rasterize(AOI_60m, tnorm)
tnorm_masked <- tnorm * mask

writeRaster(chm,filename = file.path(pathtoRasterOutputs,"hhm_chm"), format="GTiff",overwrite=TRUE)

#===============================================================================
#=====================filtering with premonsoon NDVI============================
#===============================================================================

NDVI <- raster(pathtoNDVIrast)
NDVI_sp <- projectRaster(NDVI, tnorm_masked)
NDVI_fil <- mask(NDVI_sp, tnorm_masked)
NDVI_valf <- NDVI_fil
NDVI_valf[NDVI_valf >= .6] <- NA
NDVI_valf[NDVI_valf <= .15] <- NA
plot(NDVI_valf)
writeRaster(NDVI_valf,filename = file.path(pathtoRasterOutputs,"NDVI_test3"), format="GTiff",overwrite=TRUE)

#===============================================================================
#=====================filtering with premonsoon EVI=============================
#===============================================================================

PSS <- rast(pathtoPSS)
EVI <- (2.5*(PSS[[4]] - PSS[[3]])) / (PSS[[4]] + (6*PSS[[3]]) - (7.5*PSS[[1]]) + 1)
EVI <- raster::raster(EVI)

EVI_sp <- projectRaster(EVI, tnorm_masked)
EVI_fil <- mask(NDVI_sp, NDVI_valf)

EVI_valf <- EVI_fil
EVI_valf[EVI_valf <= 0.35 | EVI_valf >= 1.2] <- NA
plot(EVI_valf)

writeRaster(EVI_valf,filename = file.path(pathtoRasterOutputs,"EVI_test2"), format="GTiff",overwrite=TRUE)

#===============================================================================
#=====================filtering with winter NGRDI=============================
#===============================================================================

PSS <- rast(pathtoPSS2)
NGRDI <- ((2*PSS[[2]]) - PSS[[3]] - PSS[[1]]) / ((2*PSS[[2]]) + PSS[[3]] + PSS[[1]])
NGRDI <- raster::raster(NGRDI)

NGRDI_sp <- projectRaster(NGRDI, tnorm_masked)
NGRDI_fil <- mask(NGRDI_sp, EVI_valf)

NGRDI_valf <- NGRDI_fil
NGRDI_valf[NGRDI_valf <= -0.04 | NGRDI_valf >= -0.01] <- NA
plot(NGRDI_valf)

writeRaster(NGRDI_valf,filename = file.path(pathtoRasterOutputs,"NGRDI_test2"), format="GTiff",overwrite=TRUE)

