library(terra)
library(tidyverse)

setwd("C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/TV_ARDO")
pathtoPlanetRasters <- "./data/PlanetScopeScenes/dec20_Jul21/PSScene"  # Folder with all PSScene data, including rasters
pathtoNDVIrasters <- "./data/Dec20_Jul21_NDVI_rasters"

#create raster stack for planetscope scene rasters from jan 2020 to jan 2021
#identify file pattern to pull only tif files from the Planet output folder
allRastFiles <- list.files(pathtoPlanetRasters, pattern = "^\\d{8}_\\d{6}_\\d{2}_\\w+_3B_AnalyticMS_SR_clip\\.tif$", full.names = TRUE)
#use pattern to create a dataframe with tifs and tif dates
PSStifs <- tibble(filePath = allRastFiles) %>%
  mutate(fileName = basename(filePath),
         year = str_extract(fileName, "\\d{4}"),
         month = str_sub(fileName, start = 5, end = 6),
         day = str_sub(fileName, start = 7, end = 8),
         date = make_date(year, month, day)) %>%
  arrange(date)

#Process rasters to obtain NDVI and save data to a new folder
for (i in 1:nrow(PSStifs)){
  #reading in raster
  PSSrast <- rast(PSStifs$filePath[i])
  #calculating NDVI
  NDVIrast <- (PSSrast[[4]] - PSSrast[[3]]) / (PSSrast[[4]] + PSSrast[[3]])
  #save resulting raster of NDVI to specified folder
  #specify file name pattern for raster outputs
  NDVI_filename <- file.path(pathtoNDVIrasters, paste0("NDVI_", PSStifs$date[i], ".tif"))
  
  writeRaster(NDVIrast, filename = NDVI_filename, overwrite = TRUE)
  
  #see confirmation of creation for each new tif
  total_iterations <- nrow(PSStifs)
  cat("Progress:", i, "/", total_iterations, "\n")
  
}




