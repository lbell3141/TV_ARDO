#Logistic regression model
#binary outcome based on continuous variables
library(readr)
library(tidymodels)
library(glmnet)
library(car)
library(caret)
library(LogicReg)
library(ggcorrplot)
library(MASS)

pathtoPreparedData <- "./data/model/rast_model/TT_data.csv"
pathtoModelOutput <- "./data/model/log_reg_model_kcfv.rds"

#load input data for regression model
#assign target variable to factor
LRM_data <- read.csv(pathtoPreparedData, header = T, sep = ",")
LRM_data$present <- as.factor(LRM_data$present)



corr_matrix <- cor(LRM_data[, 5:ncol(LRM_data)])
corr_matrix <- cor(LRM_data[model_vars])
# summarize the correlation matrix
print(round(corr_matrix,2))
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(corr_matrix, cutoff=0.75, names=TRUE)
highlyCorrelated <- highlyCorrelated[!highlyCorrelated %in% c("NDVI_2021.02.09.tif", "NDVI_2021.04.10.tif", "NDVI_2021.06.01.tif")]
ggcorrplot(corr_matrix, type = "lower", lab = TRUE)



#split data into testing and training data
#set seed to make random values reproducible 
#split data and assign 80% to train and 20% to test
set.seed(333)
split_data <- initial_split(LRM_data, prop = 0.2, strata = present)
train_data <- split_data %>%
  training()
test_data <- split_data %>%
  testing()

#select variables for model; remove variables with high correlation
model_vars <- names(LRM_data)[5:ncol(LRM_data)]
model_vars <- model_vars[!model_vars %in% highlyCorrelated]
model_formula <- paste("present ~ ", paste(model_vars, collapse = " + "), sep = "")
model_formula <-formula(model_formula)
#===============================================================================
#===============================================================================
##Adding k-fold cross validation for training##
#specify k fold; cv = cross validation; 10 = number of folds 
index <- createDataPartition(LRM_data$present, p=.8, list=FALSE, times=1)
train_df <- LRM_data[index,]
test_df <- LRM_data[-index,]

train_folds <- trainControl(method = "cv",
                            number = 10,
                            savePredictions = "all",
                            classProbs = T)
#make model with kfoldcv
model_k <- train(model_formula, data = train_data,
                 method = "glm",
                 family = binomial,
                 trControl = train_folds)
model <- model_k
#===============================================================================
#===============================================================================
model <- lm(model_formula, data = LRM_data)
AIC(model)

#train model
model <- logistic_reg(mixture = double(1), penalty = double(1))%>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(model_formula, data = train_data)

#see variable summary from trained model
var_summary <- tidy(model)

#see training data predictions
#class prediction returns Y/N; prob prediction returns a prob value for Y or N designation
pred_class <- predict(model,
                      new_data = test_data,
                      type = "class")
pred_proba <- predict(model,
                      new_data = test_data,
                      type = "prob")
results <- test_data %>%
  select(present) %>%
  bind_cols(pred_class, pred_proba)


#evaluate model
accuracy(results, truth = present, estimate = .pred_class)

#confusion matrix
conf_mat(results, truth = present,
         estimate = .pred_class)  
  
saveRDS(model, file = pathtoModelOutput)

#===========Multicollinearity =============================
model <- glm(model_formula, data = train_data, family = binomial)

# Calculate VIF
vif_values <- car::vif(model)

# Display VIF values
print(vif_values < 10)
#==========================================================
#==============hyperparamter tuning========================
#==========================================================
#sample
# Define the logistic regression model with penalty and mixture hyperparameters
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# Define the grid search for the hyperparameters
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

# Define the workflow for the model
log_reg_wf <- workflow() %>%
  add_model(log_reg) %>%
  add_formula(model_formula)

# Define the resampling method for the grid search
folds <- vfold_cv(train_data, v = 5)

# Tune the hyperparameters using the grid search
log_reg_tuned <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(save_pred = TRUE)
)

select_best(log_reg_tuned, metric = "roc_auc")

# Fit the model using the optimal hyperparameters
log_reg_final <- logistic_reg(penalty = 0.0000000001, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(model_formula, data = train_data)

# Evaluate the model performance on the testing set
pred_class <- predict(log_reg_final,
                      new_data = test_data,
                      type = "class")
results <- test_data %>%
  select(present) %>%
  bind_cols(pred_class, pred_proba)

# Create confusion matrix
conf_mat(results, truth = present,
         estimate = .pred_class)

precision(results, truth = present,
          estimate = .pred_class)

#=========================choosing vars
# Initialize an empty vector to store selected variables
selected_vars <- character()

# Initialize a variable to store the current model AIC
best_AIC <- Inf

# Start with an empty formula
current_formula <- formula("present ~ 1")

# Loop through each variable
for (var in setdiff(names(LRM_data), "present")) {  # Exclude the dependent variable
  # Fit the model with the selected variables plus the current variable
  temp_formula <- update(current_formula, paste(". ~ . + ", var))
  model <- lm(temp_formula, data = LRM_data)
  
  # Check the AIC of the current model
  current_AIC <- AIC(model)
  
  # If the AIC of the current model is lower than the best AIC so far, update selected variables and best AIC
  if (current_AIC < best_AIC) {
    selected_vars <- c(selected_vars, var)
    best_AIC <- current_AIC
    current_formula <- temp_formula
  }
}

# Print the selected variables
print(selected_vars)

