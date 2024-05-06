#create a plot showing removed ARDO ploygons within Tanque Verde AOI
#ARDO polygons manually created in and exported from google earth pro 
#determining removal based on changes in average July NDVI from 2021 to 2023 for each polygon
#NDVI data calculated from Planet labs PSS product
#2021 raster stack made in NDVI_comparison.R

library(sf)
library(terra)
library(ggplot2)

pathtoshpfile <-  "./data/indiv_poly/indiv_poly-polygon.shp"
pathto2021rasts <- "./data/20_21_NDVI_stack.tif"
pathto2023rasts <- "./data/22_23_NDVI_stack.tif"
pathtoHM20rast <- "./data/hm_NDVI_Nov20.tif"
pathtoHM23rast <- "./data/hm_NDVI_Sept23.tif"


#load data
ARDO_sf <- vect(pathtoshpfile)
stack_21 <- rast(pathto2021rasts)
stack_23 <- rast(pathto2023rasts)

#reproject sf to raster crs
ARDO_sfcrs <- project(ARDO_sf, stack_21)

#find average value of pixels contained within each ARDO polygon
#make sure GEP file is not multigeometry
avg21_poly_vals <- extract(stack_21, ARDO_sfcrs, fun = mean, na.rm = TRUE)
avg23_poly_vals <- extract(stack_23, ARDO_sfcrs, fun = mean, na.rm = TRUE)

#compare values to determine removal

NDVI_21 <- avg21_poly_vals[,43]
NDVI_21 <- as.data.frame(NDVI_21)
NDVI_23 <- avg23_poly_vals[,149]
NDVI_23 <- as.data.frame(NDVI_23)
comp_df <- cbind(NDVI_21, NDVI_23)
comp_df$difference <- comp_df$NDVI_23 - comp_df$NDVI_21

plot_comp <- ggplot(data = comp_df, mapping = aes(x = seq_along(NDVI_21))) +
  geom_point(aes(y = NDVI_21, color = "NDVI_21")) +
  geom_point(aes(y = NDVI_23, color = "NDVI_23")) +
  scale_x_continuous(breaks = seq(0, 105, by = 2)) +
  labs(x = "Polygon #", y = "NDVI", color = "Variable") +
  scale_color_manual(values = c("NDVI_21" = "blue", "NDVI_23" = "red")) +
  theme_minimal()
plot_comp

plot_dif <- ggplot(data = comp_df, mapping = aes(x = seq_along(NDVI_21))) +
  geom_point(aes(y = difference)) +
  scale_x_continuous(breaks = seq(0, 105, by = 2)) +
  labs(x = "Polygon #", y = "Difference in NDVI (2023 - 2021)") +
  theme_minimal()
plot_dif

comp_df$removed <- comp_df$difference < 0.1

#===================with 0.5m data==============================================
#compares Nov 2020 to Sept 2023

#load data
hm20 <- rast(pathtoHM20rast)
hm23 <- rast(pathtoHM23rast)

hm_avg20_poly_vals <- extract(hm20, ARDO_sfcrs, fun = mean, na.rm = TRUE)
hm_avg23_poly_vals <- extract(hm23, ARDO_sfcrs, fun = mean, na.rm = TRUE)

hm_comp_df <- cbind(hm_avg20_poly_vals, hm_avg23_poly_vals)
colnames(hm_comp_df) <- c("ID", "NDVI_20", "ID2", "NDVI_23")
hm_comp_df <- subset(hm_comp_df, select = -ID2)
hm_comp_df$difference <- hm_comp_df$NDVI_23 - hm_comp_df$NDVI_20

hm_plot_comp <- ggplot(data = hm_comp_df, mapping = aes(x = ID)) +
  geom_point(aes(y = NDVI_20, color = "NDVI_20")) +
  geom_point(aes(y = NDVI_23, color = "NDVI_23")) +
  labs(x = "Polygon #", y = "0.5m NDVI", color = "Variable") +
  scale_x_continuous(breaks = seq(0, max(hm_comp_df$ID), by = 2)) +
  scale_color_manual(values = c("NDVI_20" = "blue", "NDVI_23" = "red")) +
  theme_minimal()
hm_plot_comp

hm_plot_dif <- ggplot(data = hm_comp_df, mapping = aes(x = ID)) +
  geom_point(aes(y = difference)) +
  scale_x_continuous(breaks = seq(0, max(hm_comp_df$ID), by = 2)) +
  labs(x = "Polygon #", y = "Difference in 0.5m NDVI (2023 - 2021)") +
  theme_minimal()
hm_plot_dif

