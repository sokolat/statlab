library(tidymodels)
library(ggplot2)
library(randomForest)
library(gbm)
library(caret)
library(xgboost)
library(table1)
library(car)
library(pROC)
library(glmnet)

set.seed(123)

data <- read.table("/Users/rango/documents/stt3781/project1/data.txt", header = TRUE, sep = "\t")
data$CARDIA <- factor(data$CARDIA)

barplot(counts, main = "Class Frequency", xlab = "Class", ylab = "Frequency", col = "lightgreen")

split <- initial_split(data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

rf_model <- rand_forest(mtry = 3, trees = 500, min_n = 8) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

recipe <- recipe(CARDIA ~ ., data = test_data)

workflow_rf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_model)

#cv_folds <- vfold_cv(train_data, v = 5)

#rf_grid <- grid_regular(
#  mtry(range = c(2, 11)),
#  min_n(range = c(5, 20))
#)

#rf_tune_results <- tune_grid(
#  workflow_rf,
#  resamples = cv_folds,
#  grid = rf_grid,
#  metrics = metric_set(yardstick::accuracy)
#)

#rf_tune_results %>% autoplot()

#best_params <- rf_tune_results %>% select_best(metric = "accuracy")
#final_rf <- finalize_workflow(workflow_rf, best_params)

#final_rf_fit <- final_rf %>% fit(data = train_data)

rf_fit <- fit(workflow_rf, data = train_data)

rf_predictions <- predict(rf_fit, new_data = test_data)

rf_results <- bind_cols(test_data, rf_predictions)

rf_conf_mat <- conf_mat(rf_results, truth = CARDIA, estimate = .pred_class)
print(rf_conf_mat)

rf_accuracy <- accuracy(rf_results, truth = CARDIA, estimate = .pred_class)

gb_model <- boost_tree(
  trees = 1000,
  mtry = 3,
  learn_rate = 0.01,
  tree_depth = 3
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

workflow_gb <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(gb_model)

#gb_grid <- grid_regular(
#  trees(range = c(500, 1000)),      
#  tree_depth(range = c(1, 5)),    
#  learn_rate(range = c(0.01, 0.1)),  
#  mtry(range = c(3,11))
#)

#gb_tune_results <- tune_grid(
#  workflow_gb,
#  resamples = cv_folds,
#  grid = gb_grid,
#  metrics = metric_set(yardstick::accuracy)
#)

#gb_tune_results %>% autoplot()

#gb_best_params <- gb_tune_results %>% select_best(metric = "accuracy")
#print(gb_best_params)
#final_gb <- finalize_workflow(workflow_gb, gb_best_params)

#final_gb_fit <- final_gb %>% fit(data = train_data)

gb_fit <- fit(workflow_gb, data = train_data)

gb_predictions <- predict(gb_fit, new_data = test_data)

gb_results <- bind_cols(test_data, gb_predictions)

gb_conf_mat <- conf_mat(gb_results, truth = CARDIA, estimate = .pred_class)

gb_accuracy <- accuracy(gb_results, truth = CARDIA, estimate = .pred_class)

form <- CARDIA ~ I(AGE^2) + factor(PHYS) + SES + I(PSYST^2)+  factor(FUMEUR) +factor(SEXE)
orig_model <- glm(formula = form, family = "binomial", data = train_data)

orig_prob <- predict(orig_model, type = "response")
apparent_roc <- roc(train_data$CARDIA, orig_prob)
apparent_auc <- auc(apparent_roc)

orig_pred <- ifelse(orig_prob > 0.5, 1, 0)
conf_matrix <- confusionMatrix(factor(orig_pred), factor(train_data$CARDIA))
apparent_f1 <- conf_matrix$byClass["F1"]

bootstrap_performance <- function(train_data, indices) {
  
  bootstrap_data <- train_data[indices, ]
  bootstrap_test_data <- train_data[-unique(indices), ]  
  
  bootstrap_model <- glm(formula = form, family = "binomial", data = bootstrap_data)
  
  bootstrap_prob <- predict(bootstrap_model, newdata = bootstrap_data, type = "response")
  bootstrap_auc <- auc(roc(bootstrap_data$CARDIA, bootstrap_prob))
  
  bootstrap_pred <- ifelse(bootstrap_prob > 0.5, 1, 0)
  conf_matrix_bootstrap <- confusionMatrix(factor(bootstrap_pred), factor(bootstrap_data$CARDIA))
  bootstrap_f1 <- conf_matrix_bootstrap$byClass["F1"]
  
  if (nrow(bootstrap_test_data) > 0) {  
    test_prob <- predict(bootstrap_model, newdata = bootstrap_test_data, type = "response")
    test_auc <- auc(roc(bootstrap_test_data$CARDIA, test_prob))
    
    test_pred <- ifelse(test_prob > 0.5, 1, 0)
    conf_matrix_test <- confusionMatrix(factor(test_pred), factor(bootstrap_test_data$CARDIA))
    test_f1 <- conf_matrix_test$byClass["F1"]
  } else {
    test_auc <- NA
    test_f1 <- NA
  }
  
  return(c(bootstrap_auc, test_auc, bootstrap_f1, test_f1))
}

n_bootstrap <- 1000
bootstrap_results <- replicate(n_bootstrap, {
  indices <- sample(1:nrow(train_data), replace = TRUE)
  bootstrap_performance(train_data, indices)
})

bootstrap_auc_avg <- mean(bootstrap_results[1, ], na.rm = TRUE)
test_auc_avg <- mean(bootstrap_results[2, ], na.rm = TRUE)
bootstrap_f1_avg <- mean(bootstrap_results[3, ], na.rm = TRUE)
test_f1_avg <- mean(bootstrap_results[4, ], na.rm = TRUE)

optimism_auc <- bootstrap_auc_avg - test_auc_avg
corrected_auc <- apparent_auc - optimism_auc

optimism_f1 <- bootstrap_f1_avg - test_f1_avg
corrected_f1 <- apparent_f1 - optimism_f1

cat("Apparent Performance (AUC):", round(apparent_auc, 3), "\n")
cat("Bootstrap Performance (AUC):", round(bootstrap_auc_avg, 3), "\n")
cat("Test Performance (AUC):", round(test_auc_avg, 3), "\n")
cat("Optimism-Corrected Performance (AUC):", round(corrected_auc, 3), "\n\n")

cat("Apparent Performance (F1-Score):", round(apparent_f1, 3), "\n")
cat("Bootstrap Performance (F1-Score):", round(bootstrap_f1_avg, 3), "\n")
cat("Test Performance (F1-Score):", round(test_f1_avg, 3), "\n")
cat("Optimism-Corrected Performance (F1-Score):", round(corrected_f1, 3), "\n")

train_data$CARDIA <- factor(train_data$CARDIA, levels = c(0, 1), labels = c("No", "Yes"))

f1_score <- function(data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = lev[2])
  recall <- sensitivity(data$pred, data$obs, positive = lev[2])
  f1 <- 2 * (precision * recall) / (precision + recall)
  c(F1 = f1)
}

custom_summary <- function(data, lev = NULL, model = NULL) {
  c(twoClassSummary(data, lev, model), f1_score(data, lev, model))
}

cv_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE,
  summaryFunction = custom_summary
)

glm_model <- train(
  form,
  data = train_data,
  method = "glm",
  family = binomial,
  trControl = cv_control,
  metric = "ROC"
)

auc_results <- glm_model$resample$ROC
f1_results <- glm_model$resample$F1

cat("Mean AUC:", mean(auc_results, na.rm = TRUE), "\n")
cat("Standard Deviation of AUC:", sd(auc_results, na.rm = TRUE), "\n")
cat("Mean F1-score:", mean(f1_results, na.rm = TRUE), "\n")
cat("Standard Deviation of F1-score:", sd(f1_results, na.rm = TRUE), "\n")

test_probs <- predict(glm_model, newdata = test_data, type = "prob")$Yes
test_preds <- ifelse(test_probs > 0.5, "Yes", "No")
test_preds <- factor(test_preds, levels = c("No", "Yes"))
test_roc <- roc(test_data$CARDIA, test_probs)
test_auc <- auc(test_roc)

test_conf_matrix <- confusionMatrix(test_preds, test_data$CARDIA, positive = "Yes")
test_f1 <- test_conf_matrix$byClass["F1"]

cat("\n==== Test Set Performance ====\n")
cat("AUC (Test Set):", round(test_auc, 3), "\n")
cat("F1-score (Test Set):", round(test_f1, 3), "\n")

cat("\n==== Classification Table (Confusion Matrix) ====\n")
