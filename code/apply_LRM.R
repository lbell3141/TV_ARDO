#applying logistic regression model to an AOI

library(Matrix)
library(raster)
library(sp)

pathtoModel <- "./data/model/log_reg_model.rds"
pathtoPreparedData <- "./data/model/train_test_data_full_AOI.csv"
pathtoRastOutput <- "./data/model/outputs/test_output.tif"

#load data
model <- readRDS(file = pathtoModel)
LRM_data <- read.csv(pathtoPreparedData, header = T, sep = ",")

#run model
output <- predict(model, LRM_data)

#combine prediction with x and y in new dataframe
coord_data <- LRM_data[, c(2,3)]
LRM_pts <- cbind(coord_data, output)

#convert to raster and check 
output_rast <- rasterFromXYZ(LRM_pts)
plot(output_rast)

#add crs manually with reference raster
projection(output_rast) <- "+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs"

#save raster
writeRaster(output_rast, pathtoRastOutput,overwrite=TRUE)

