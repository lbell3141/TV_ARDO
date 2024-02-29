library(terra)
library(tidyverse)

setwd("C:/Users/lindseybell/OneDrive - University of Arizona/Desktop/ARDO")
pathto2020rasters <- "./data/2020_full_psscene_analytic_udm2/PSScene"  # Folder with all PSScene data, including rasters
pathto2020NDVIrasters <- "./data/2020_NDVI_rasters"

#create raster stack for planetscope scene rasters from jan 2020 to jan 2021
#identify file pattern to pull only tif files from the Planet output folder
allRastFiles <- list.files(pathto2020rasters, pattern = "\\d{8}_\\d{6}_\\w+_3B_AnalyticMS_clip\\.tif$", full.names = TRUE)
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
  NDVIrast <- (PSSrast[[3]] - PSSrast[[4]]) / (PSSrast[[3]] + PSSrast[[4]])
  #save resulting raster of NDVI to specified folder
  #specify file name pattern for raster outputs
  NDVI_filename <- file.path(pathto2020NDVIrasters, paste0("NDVI_", PSStifs$date[i], ".tif"))
  
  writeRaster(NDVIrast, filename = NDVI_filename, overwrite = TRUE)
  
  #see confirmation of creation for each new tif
  cat("NDVI raster for", PSStifs$date[i], "has been created.\n")
  
}




