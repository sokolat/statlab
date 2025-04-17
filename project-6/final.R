library(ggplot2)
library(broom)
library(dplyr)

data <- read.table("/Users/rango/documents/statlab/project-6/patients.txt", header = TRUE, sep = " ")
data$sexe <- factor(data$sexe)
data$traitement <- factor(data$traitement)

ggplot(data, aes(x = data$sexe, y = data$taille, fill = data$sexe)) +
  geom_boxplot() +
  labs(title = "Taille par Sexe",
       x = "Sexe",
       y = "Taille") +
  theme_minimal()

male.sizes <- data[data$sexe == 0, "taille"]
female.sizes <- data[data$sexe == 1, "taille"]

t.test(male.sizes, female.sizes, data=data, var.equal = TRUE)

fit <- lm(formula=taille ~ age, data=data)
summary(fit)

data.with.ps <- glm(formula=traitement ~ age + sexe + severite, family='binomial', data=data) |>
  augment(type.predict = "response", data = data) |>
  rename(pscore = .fitted)

data.with.ps <- data.with.ps |>
  mutate(weight = ifelse(traitement == 1, 
                         1 / pscore, 
                         1 / (1 - pscore)))

data.with.ps |>
  group_by(traitement) |>
  summarise(weighted_mean_size = weighted.mean(taille, weight))

mean.radio <- with(data.with.ps[data.with.ps$traitement == 1, ],
                     weighted.mean(taille, weight))
mean.chimio <- with(data.with.ps[data.with.ps$traitement == 0, ],
                       weighted.mean(taille, weight))

causal_effect <- mean.radio - mean.chimio
causal_effect

set.seed(123)
n_boot <- 1000
boot_estimates <- numeric(n_boot)

for (i in 1:n_boot) {
  # Resample data with replacement
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  
  # Re-estimate propensity scores in bootstrap sample
  ps_model <- glm(traitement ~ age + sexe + severite, 
                  family = "binomial", 
                  data = boot_sample)
  
  boot_sample$pscore <- predict(ps_model, type = "response")
  
  # Recompute IPTW weights
  boot_sample$weight <- ifelse(boot_sample$traitement == 1,
                               1 / boot_sample$pscore,
                               1 / (1 - boot_sample$pscore))
  
  # Compute weighted means
  mean_treated <- with(boot_sample[boot_sample$traitement == 1, ],
                       weighted.mean(taille, weight))
  mean_untreated <- with(boot_sample[boot_sample$traitement == 0, ],
                         weighted.mean(taille, weight))
  
  # Store causal effect estimate
  boot_estimates[i] <- mean_treated - mean_untreated
}

# Compute 95% confidence interval
ci_lower <- quantile(boot_estimates, 0.025)
ci_upper <- quantile(boot_estimates, 0.975)
causal_effect <- mean(boot_estimates)

# Output
list(
  causal_effect = causal_effect,
  CI_95 = c(lower = ci_lower, upper = ci_upper)
)
