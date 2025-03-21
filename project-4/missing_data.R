# Loading necessary libraries
library(zoo)      # Provides functions for time series objects
library(mice)     # Implements multiple imputation for missing data
library(tidyr)    # Provides functions for tidying data (e.g., reshape)
library(purrr)    # Implements functional programming tools (e.g., map)

# Set seed for reproducibility of results
set.seed(123)

# Read in datasets from files
addh.incomp <- read.table("/Users/rango/documents/statlab/project-4/addh_incomp.txt", header = TRUE, sep = "\t")  # Incomplete data
addh.comp <- read.table("/Users/rango/documents/statlab/project-4/addh_comp.txt", header = TRUE, sep = "\t")  # Complete data

# Convert specified columns in the complete dataset to factors (categorical variables)
addh.comp$psychotherapie <- factor(addh.comp$psychotherapie)
addh.comp$sexe <- factor(addh.comp$sexe)
addh.comp$fumeur <- factor(addh.comp$fumeur)

# Reshape the complete dataset into wide format based on 'ID2' and 'vague_temps'
addh.comp.wide <- reshape(addh.comp, idvar="ID2", timevar="vague_temps", direction="wide")

# Set parameters for multiple imputation
num.iter <- 5      # Number of imputation iterations
max.iter <- 5      # Maximum iterations for each imputation
seed = 123         # Set seed for random number generation

# Perform multiple imputation using the mice package
multi.imp <- mice(data=addh.comp.wide, m=num.iter, maxit=max.iter, seed=seed)

# Function to reshape each imputed dataset into long format for further analysis
to.long <- function(i) {
  # Complete the i-th imputation and reshape it into long format
  df <- complete(multi.imp, i)  # Extract i-th imputation dataset
  imp.data.long <- reshape(df, direction="long", idvar="ID2", timevar="vague_temps")
}

# Apply the function to all imputed datasets and store results
multi.imp.data.long <- map(1:num.iter, to.long)

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
cat("Total variance: ", var.tot, "\n")
