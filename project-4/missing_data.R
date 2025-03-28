# Loading necessary libraries
#library(zoo)      # Provides functions for time series objects
library(mice)     # Implements multiple imputation for missing data
library(tidyr)    # Provides functions for tidying data (e.g., reshape)
library(purrr)    # Implements functional programming tools (e.g., map)
library(dplyr)
library(sandwich)
library(table1)
library(lattice)

# Set seed for reproducibility of results
set.seed(123)

addh.incomp <- read.table("/Users/rango/documents/statlab/project-4/addh_incomp.txt", header = TRUE, sep = "\t")  # Incomplete data
addh.comp <- read.table("/Users/rango/documents/statlab/project-4/addh_comp.txt", header = TRUE, sep = "\t")  # Complete data

# Convert specified columns in the complete dataset to factors (categorical variables)
addh.comp$psychotherapie <- factor(addh.comp$psychotherapie)
addh.comp$sexe <- factor(addh.comp$sexe)
addh.comp$fumeur <- factor(addh.comp$fumeur)
addh.comp$poids <- ifelse(addh.comp$poids > 500, 500, addh.comp$poids)
addh.comp$poids <- ifelse(addh.comp$poids < 50, 50, addh.comp$poids)
addh.comp$sentiment_deprime <- factor(addh.comp$sentiment_deprime)
 
# Reshape the complete dataset into wide format based on 'ID2' and 'vague_temps'
addh.comp.wide <- reshape(addh.comp, idvar="ID2", timevar="vague_temps", direction="wide")
 
addh.comp.wide$sexe <- addh.comp.wide$sexe.1
addh.comp.wide <- select(addh.comp.wide, -c(sexe.1, sexe.2, sexe.3, sexe.4))
addh.comp.wide$age <- addh.comp.wide$age.1
addh.comp.wide <- select(addh.comp.wide, -c(age.1, age.2, age.3, age.4))
addh.comp.wide$SSE <- addh.comp.wide$SSE.1
addh.comp.wide <- select(addh.comp.wide, -c(SSE.1, SSE.2, SSE.3, SSE.4))
 
meth <- make.method(addh.comp.wide)
#meth['age'] <- 'norm'
# Create the predictor matrix for the dataset 'addh.comp.wide'
# The make.predictorMatrix function generates a matrix of 1s and 0s 
# indicating which variables are used as predictors for each other
pred.mat <- make.predictorMatrix(addh.comp.wide)
 
# Set the entire column corresponding to 'ID2' in the predictor matrix to 0
# This will exclude 'ID2' from being used as a predictor variable in the imputation process
pred.mat[, 'ID2'] <- 0
 
# Set parameters for multiple imputation
num.iter <- 20    # Number of imputation iterations
max.iter <- 5     # Maximum iterations for each imputation
seed = 123         # Set seed for random number generation
 
# Perform multiple imputation using the mice package
multi.imp <- mice(data=addh.comp.wide, m=num.iter, maxit=max.iter, seed=seed, predictorMatrix = pred.mat, method=meth)
 
# Generate the density plot with improved formatting
densityplot(multi.imp, 
            layout = c(4, 4), 
            main = "Diagramme de densité des valeurs imputées",
            lwd = 2,
            ylab="Densité",
            strip = strip.custom(factor.levels = c("Verres d’alcool t=1", "Sentiment de dépression t=1", 
                                                   "Poids t=1", "Verres d’alcool t=2",
                                                   "Sentiment de dépression t=2", "Poids t=2", 
                                                   "Verres d’alcool t=3", "Sentiment de dépression t=3", 
                                                   "Poids t=3", "Verres d’alcool t=4",
                                                   "Sentiment de déprime t=4", "Poids t=4", 
                                                   "Âge", "Statut socio-économique")))
 
# Function to reshape each imputed dataset into long format for further analysis
 
#to.long <- function(i) {
#  # Complete the i-th imputation and reshape it into long format
#  df <- complete(multi.imp, i)  # Extract i-th imputation dataset
#  imp.data.long <- reshape(df, direction="long", idvar="ID2", timevar="vague_temps")
#}
 
to.long <- function(i) {
  df <- complete(multi.imp, i)
  df <- df %>%
    mutate(
      sexe.1 = sexe, sexe.2 = sexe, sexe.3 = sexe, sexe.4 = sexe,
      age.1 = age, age.2 = age, age.3 = age, age.4 = age,
      SSE.1 = SSE, SSE.2 = SSE, SSE.3 = SSE, SSE.4 = SSE
    ) %>%
    select(-sexe, -age, -SSE) 
  imp.data.long <- reshape(df, direction="long", idvar="ID2", timevar="vague_temps")
  return(imp.data.long)
}
 
# Apply the function to all imputed datasets and store results
multi.imp.data.long <- map(1:num.iter, to.long)
 
table1(~ alcool_nb_verres.1 + factor(sentiment_deprime.1) + age.1 + factor(sexe.1) + SSE.1 + factor(fumeur.1) + poids.1 + factor(psychotherapie.1) + factor(vague_temps), data=data.frame(multi.imp.data.long[1]))
 
# Define the linear model formula with multiple predictor variables
formula <- alcool_nb_verres.1 ~ vague_temps + psychotherapie.1 + sentiment_deprime.1 + age.1 + sexe.1 + SSE.1 + fumeur.1 + poids.1
 
# Function to fit the linear model and extract coefficient and variance for 'psychotherapie.11'
fit <- function(i) {
  # Select the i-th imputed dataset
  df <- multi.imp.data.long[[i]]
  # Fit a linear model
  model <- lm(formula, data=df)
  # Extract the coefficient for 'psychotherapie.11' (psychotherapy treatment at time 1)
  beta.est <- coef(model)['psychotherapie.11']
  # Extract the variance of the coefficient 'psychotherapie.11'
  var.est <- vcov(model)['psychotherapie.11', 'psychotherapie.11']
  # Return both the estimated coefficient and its variance
  c(beta.est, var.est)
}
 
# Apply the fitting function to each imputed dataset and store results
results <- map(1:num.iter, fit)
 
# Initialize vectors to store the estimated coefficients and variances for each iteration
betas.est <- numeric(num.iter)
vars.est <- numeric(num.iter)
 
# Extract the coefficients and variances from the results
for (i in 1:num.iter) {
  betas.est[i] <- results[[i]][[1]]  # Store estimated coefficient
  vars.est[i] <- results[[i]][[2]]   # Store estimated variance
}
 
# Calculate the mean of the estimated coefficients across all imputations
betas.est.mean <- mean(betas.est)
 
# Calculate intra-imputation variance (mean of individual variances)
var.intra <- mean(vars.est)
 
# Calculate inter-imputation variance (variance between imputed estimates)
var.inter <- mean((betas.est - betas.est.mean) ** 2)
 
# Calculate the total variance (intra + inter variance adjusted for number of imputations)
var.tot <- var.intra + var.inter * (1 + 1/num.iter)
 
cat("Estimated effect of 'psychotherapie.11': ", betas.est.mean, "\n")
cat("Intra-imputation variance: ", var.intra, "\n")
cat("Inter-imputation variance: ", var.inter, "\n")

SE <- sqrt(var.tot)
nu <- (num.iter - 1) * (1 + (var.intra / ((1 + 1/num.iter) * var.inter)))^2

alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = nu)
CI_lower <- betas.est.mean - t_crit * SE
CI_upper <- betas.est.mean + t_crit * SE

cat("Pooled estimate:", round(betas.est.mean, 3), "\n")
cat("95% Confidence Interval: (", round(CI_lower, 3), ",", round(CI_upper, 3), ")\n")
cat("Total variance: ", var.tot, "\n")