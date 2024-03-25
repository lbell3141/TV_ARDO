library(terra)
library(sf)

Planet_AOI <- "./data/TV_AOI/TV_AOI_Houghton_to_GuestRanch.geojson"
AOI <- "./data/TV_AOI/TV_buff_60m.shp"
outputpath <- "./data/TV_AOI/TV_repro.shp"

Planet_AOi_vect <- vect(Planet_AOI)
AOI_shp <- st_read(new_AOI)

Planet_crs <- crs(Planet_AOi_vect)
 
repro_shp <- st_transform(AOI_shp, crs = Planet_crs)

repro_shp <- repro_shp[1,3]
plot(repro_shp)                  

st_write(repro_shp, outputpath)
