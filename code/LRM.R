#Logistic regression model
#binary outcome based on continuous variables
library(readr)
library(tidymodels)
library(glmnet)
library(car)
library(caret)
library(LogicReg)
library(ggcorrplot)
library(leaps)

pathtoPreparedData <- "./data/model/rast_model/TT_data_rastmodel.csv"
pathtoModelOutput <- "./data/model/log_reg_model_rastmodel.rds"

#load input data for regression model
#assign target variable to factor
LRM_data <- read.csv(pathtoPreparedData, header = T, sep = ",")
LRM_data$present <- as.factor(LRM_data$present)

model_formula <- "present ~ height + dist_bank + NDVI_2021.04.10.tif + NDVI_2021.06.01.tif + NDVI_2021.07.30.tif + NDVI_2021.05.15.tif"
model_formula <- formula(model_formula)

set.seed(333)
split_data <- initial_split(LRM_data, prop = 0.2, strata = present)
train_data <- split_data %>%
  training()
test_data <- split_data %>%
  testing()

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


#===============================================================================

#corr_matrix <- cor(LRM_data[, 5:ncol(LRM_data)])
#corr_matrix <- cor(LRM_data[model_vars])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(corr_matrix, cutoff=0.75, names=TRUE)
highlyCorrelated <- highlyCorrelated[!highlyCorrelated %in% c( "NDVI_2021.04.10.tif", "NDVI_2021.06.01.tif")]

#select variables for model; remove variables with high correlation
model_vars <- names(LRM_data)[5:ncol(LRM_data)]
model_vars <- model_vars[!model_vars %in% highlyCorrelated]
model_formula <- paste("present ~ ", paste(model_vars, collapse = " + "), sep = "")
model_formula <-formula(model_formula)

model_formula <- "present ~ height + dist_bank + NDVI_2021.04.10.tif + NDVI_2021.06.01.tif + NDVI_2021.07.30.tif + NDVI_2021.05.15.tif"
model_formula <- formula(model_formula)

model_vars <- c("height", "dist_bank", "NDVI_2021.04.10.tif", "NDVI_2021.07.30.tif")
corr_matrix <- cor(LRM_data[model_vars])
#corr_matrix <- cor(LRM_data[model_vs])
ggcorrplot(corr_matrix, type = "lower", lab = TRUE)


#get all combos of vars and determine best model
all_combinations <- regsubsets(model_formula, data = LRM_data, nvmax = 60, really.big = TRUE)
best_model <- which.min(summary(all_combinations)$bic)
selected_vars <- names(coef(all_combinations, id = best_model))
print(selected_vars)


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
set.seed(123)

#error from present level names... change values to "something meaningful"
LRM_data$present <- ifelse(LRM_data$present == 0, "no", "yes")

index <- createDataPartition(LRM_data$present, p = 0.8, list = FALSE, times = 1)
train_df <- LRM_data[index, ]
test_df <- LRM_data[-index, ]

train_folds <- trainControl(method = "cv",
                            number = 15,
                            savePredictions = "all",
                            classProbs = TRUE)

# Make model with k-fold cross-validation
model_k <- train(present ~ height + dist_bank + NDVI_2021.04.10.tif + NDVI_2021.06.01.tif + NDVI_2021.07.30.tif + NDVI_2021.05.15.tif, 
                 data = train_df,
                 method = "glm",
                 family = binomial(),
                 trControl = train_folds)

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
print(vif_values < 8.5)
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

# Get all possible combinations of variables
all_combinations <- regsubsets(model_formula, data = LRM_data, nvmax = 60, really.big = TRUE)

# Extract the model with the lowest AIC
best_model <- which.min(summary(all_combinations)$bic)

# Get the variables in the best model
selected_vars <- names(coef(all_combinations, id = best_model))

# Print the selected variables
print(selected_vars)

