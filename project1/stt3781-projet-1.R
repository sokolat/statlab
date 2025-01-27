library(tidymodels)
library(ggplot2)
library(randomForest)
library(gbm)
library(caret)
library(xgboost)
library(table1)
library(car)
library(pROC)

set.seed(123)

data <- read.table("/Users/rango/documents/stt3781/project1/data.txt", header = TRUE, sep = "\t")
data
data$CARDIA <- factor(data$CARDIA)

counts <- table(data$CARDIA)
print(counts)

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
print(rf_accuracy)

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
print(gb_conf_mat)

gb_accuracy <- accuracy(gb_results, truth = CARDIA, estimate = .pred_class)
print(gb_accuracy)

logistic_model <- glm(formula = CARDIA ~ AGE + factor(SEXE) + PSYST + factor(PHYS) + NB_HT + factor(HIS_C) + factor(HISFAM_C) + factor(FUMEUR) + factor(DIAB) + IMC + SES, family = "binomial", data = data)
logistic_model

log_odds <- predict(logistic_model, type = "link")

ggplot(data, aes(x = AGE, y = log_odds)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Log Odds vs Age",
    x = "Age",
    y = "Log Odds"
  ) +
  theme_minimal()

ggplot(data, aes(x = PSYST, y = log_odds)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Log Odds vs PSYST",
    x = "PSYST",
    y = "Log Odds"
  ) +
  theme_minimal()

ggplot(data, aes(x = IMC, y = log_odds)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Log Odds vs IMC",
    x = "IMC",
    y = "Log Odds"
  ) +
  theme_minimal()

proportions_nb_ht <- data %>%
  group_by(NB_HT) %>%
  summarise(proportion = mean(CARDIA == 1))

ggplot(proportions_nb_ht, aes(x = NB_HT, y = proportion, color = NB_HT)) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cardiac Cases by NB_HT Modality",
    x = "NB_HT Modality",
    y = "Proportion of Cardiac Cases"
  ) +
  theme_minimal()

proportions_imc <- data %>%
  group_by(IMC) %>%
  summarise(proportion = mean(CARDIA == 1))

ggplot(proportions_imc, aes(x = IMC, y = proportion, color = IMC)) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cardiac Cases by IMC Modality",
    x = "IMC Modality",
    y = "Proportion of Cardiac Cases"
  ) +
  theme_minimal()

proportions_ses <- data %>%
  group_by(SES) %>%
  summarise(proportion = mean(CARDIA == 1))

ggplot(proportions_ses, aes(x = SES, y = proportion, color = SES)) +
  geom_point(size = 3, show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cardiac Cases by SES Modality",
    x = "SES Modality",
    y = "Proportion of Cardiac Cases"
  ) +
  theme_minimal()

vif(logistic_model)

dev_residuals <- residuals(logistic_model, type = "deviance")
predicted_probs <- predict(logistic_model, type = "response")

dev_residuals_df <- data.frame(dev_residuals)
ggplot(dev_residuals_df, aes(sample = dev_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  ggtitle("Q-Q Plot of Deviance Residuals") +
  theme_minimal()

logistic_model <- glm(formula = CARDIA ~ AGE + factor(SEXE) + factor(PHYS) + NB_HT + factor(HIS_C) + factor(HISFAM_C) + factor(FUMEUR) + factor(DIAB) + IMC + SES, family = "binomial", data = data)
logistic_model

dev_residuals <- residuals(logistic_model, type = "deviance")
predicted_probs <- predict(logistic_model, type = "response")

dev_residuals_df <- data.frame(dev_residuals)
ggplot(dev_residuals_df, aes(sample = dev_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  ggtitle("Q-Q Plot of Deviance Residuals") +
  theme_minimal()

#plot_data <- data.frame(Predicted = log_odds, DevianceResiduals = dev_residuals)

#ggplot(plot_data, aes(y = DevianceResiduals, x = log_odds)) +
#  geom_point(color = "steelblue", alpha = 0.6) +
#  labs(title = "Deviance Residuals vs Predicted Values",
#       x = "Predicted values",
#       y = "Deviance Residuals") +
#  theme_minimal()

form <- CARDIA ~ AGE + factor(SEXE) + PSYST + factor(PHYS) + NB_HT + factor(HIS_C) + factor(HISFAM_C) + factor(FUMEUR) + factor(DIAB) + IMC + SES
orig_model <- glm(formula = form, family = "binomial", data = data)

orig_prob <- predict(orig_model, type = "response")
apparent_roc <- roc(data$CARDIA, orig_prob)
apparent_auc <- auc(apparent_roc)

bootstrap_performance <- function(data, indices) {
  
  bootstrap_data <- data[indices, ]
  test_data <- data[-unique(indices), ]  
  
  bootstrap_model <- glm(formula = form, family = "binomial", data = bootstrap_data)
  
  bootstrap_prob <- predict(bootstrap_model, newdata = bootstrap_data, type = "response")
  bootstrap_auc <- auc(roc(bootstrap_data$CARDIA, bootstrap_prob))
  
  if (nrow(test_data) > 0) {  
    test_prob <- predict(bootstrap_model, newdata = test_data, type = "response")
    test_auc <- auc(roc(test_data$CARDIA, test_prob))
  } else {
    test_auc <- NA
  }
  
  return(c(bootstrap_auc, test_auc))
}

n_bootstrap <- 1000
bootstrap_results <- replicate(n_bootstrap, {
  indices <- sample(1:nrow(data), replace = TRUE)
  bootstrap_performance(data, indices)
})

bootstrap_auc_avg <- mean(bootstrap_results[1, ], na.rm = TRUE)
test_auc_avg <- mean(bootstrap_results[2, ], na.rm = TRUE)

optimism <- bootstrap_auc_avg - test_auc_avg
corrected_performance <- apparent_auc - optimism

cat("Apparent Performance (AUC):", round(apparent_auc, 3), "\n")
cat("Bootstrap Performance (AUC):", round(bootstrap_auc_avg, 3), "\n")
cat("Test Performance (AUC):", round(test_auc_avg, 3), "\n")
cat("Optimism-Corrected Performance (AUC):", round(corrected_performance, 3), "\n")

data$CARDIA <- factor(data$CARDIA, 
                      levels = c(0, 1),
                      labels = c("No", "Yes"))

n_folds <- 10
n_repeats <- 10

cv_control <- trainControl(
  method = "repeatedcv",
  number = n_folds,
  repeats = n_repeats,
  classProbs = TRUE,
  summaryFunction = twoClassSummary 
)

glm_model <- train(
  form,
  data = data,
  method = "glm",
  family = binomial,
  trControl = cv_control,
  metric = "ROC"
)

print(glm_model)

auc_results <- glm_model$resample$ROC
cat("Mean AUC:", mean(auc_results), "\n")
cat("Standard Deviation of AUC:", sd(auc_results), "\n")