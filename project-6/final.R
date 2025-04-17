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

causal.effect <- mean.radio - mean.chimio
causal.effect

set.seed(123)
n.boot <- 1000
boot.estimates <- numeric(n.boot)

for (i in 1:n.boot) {
  boot.sample <- data[sample(nrow(data), replace = TRUE), ]
  
  ps.model <- glm(traitement ~ age + sexe + severite, 
                  family = "binomial", 
                  data = boot.sample)
  
  boot.sample$pscore <- predict(ps.model, type = "response")
  
  boot.sample$weight <- ifelse(boot.sample$traitement == 1,
                               1 / boot.sample$pscore,
                               1 / (1 - boot.sample$pscore))
  
  mean.radio <- with(boot.sample[boot.sample$traitement == 1, ],
                       weighted.mean(taille, weight))
  mean.chimio <- with(boot.sample[boot.sample$traitement == 0, ],
                         weighted.mean(taille, weight))
  
  boot.estimates[i] <- mean.radio - mean.chimio
}

ci.lower <- quantile(boot.estimates, 0.025)
ci.upper <- quantile(boot.estimates, 0.975)
causal.effect <- mean(boot.estimates)

list(
  causal.effect = causal.effect,
  CI.95 = c(lower = ci.lower, upper = ci.upper)
)
