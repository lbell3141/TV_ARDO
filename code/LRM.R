#Some notes
#LiDAR cut
#premonsoon NDVI signal separation
#probability distance from bank
#probability of nearest pixel (if pixel = 1, increases prob that surrounding pixels are also 1)


#Logistic regression
#binary outcome based on continuous variables
library(readr)
library(tidymodels)

pathtoPreparedData <- "./data/LRM/LRM_prepared_data.csv"

#load input data for regression model
#assign target variable to factor
LRM_data <- read.csv("pathtoPreparedData", header = T, sep = ",")
LRM_data$present <- as.factor(LRM_data$present)

#split data into testing and training data
#set seed to make random values reproducible 
#split data and assign 80% to train and 20% to test
set.seed(333)
split_data <- initial_split(LRM_data, prop = 0.8, strata = y)
train_data <- split_data %>%
  training()
test_data <- split_data %>%
  testing()

#train model
model <- logistic_reg(mixture = double(1), penalty = double(1))%>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(present ~ ., data = train_data)

#see variable summary from trained model
tidy(model)

#see training data predictions
#class prediction returns Y/N; prob prediction returns a prob value for Y or N designation
prediction <- predict(model,
                      new_data = test_data,
                      type = "class")

#evaluate model
accuracy(prediction, truth = present, estimate = .prediction)

#confusion matrix
conf_mat(prediction, truth = present,
         estimate = .prediction)  
  
#==============hyperparamter tuning========================
# Define the logistic regression model with penalty and mixture hyperparameters
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# Define the grid search for the hyperparameters
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

# Define the workflow for the model
log_reg_wf <- workflow() %>%
  add_model(log_reg) %>%
  add_formula(present ~ .)

# Define the resampling method for the grid search
folds <- vfold_cv(train, v = 5)

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
  fit(y~., data = train)

# Evaluate the model performance on the testing set
pred_class <- predict(log_reg_final,
                      new_data = test,
                      type = "class")
results <- test %>%
  select(y) %>%
  bind_cols(pred_class, pred_proba)

# Create confusion matrix
conf_mat(results, truth = y,
         estimate = .pred_class)


