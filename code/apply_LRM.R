#applying logistic regression model to an AOI


pathtoModel <- "./data/model/log_reg_model.rds"
pathtoPreparedData <- "./data/model/train_test_data_full_AOI.csv"


model <- readRDS(file = pathtoModel)
LRM_data <- read.csv(pathtoPreparedData, header = T, sep = ",")

predict(model, LRM_data)


