# IV simulation 

## Test with single data set

library(MASS)
library(ggplot2)

# create two correlated variables x-Rest and c
xR_and_c <- mvrnorm(1000, c(2, 1.5), matrix(c(1, 0.55, 0.55, 1), 2, 2))
xR <- x_and_c[, 1]
c <- x_and_c[, 2]  # unobserved confounder

# instrument
z <- rnorm(1000)

# observed x
x <- xR + z

# create outcome based on 
y <- 1 + x + c + rnorm(1000, 0, 0.5)

dat <- data.frame(x, z, y)  # this would be the available dataset

# check if variables are related as expected
cor(xR, c)  # set at .50
cor(x, c)  # still correlated
cor(z, c)  # uncorrelated by assumption

true_model <- lm(y  ~ x + c)
biased_model <- lm(y ~ x)

x_pred <- lm(x ~ z)$fitted.values

# fit y-outcome model with predicted values of x ~ z model
iv_model <- lm(y ~ x_pred)


### repeat with simulations

sim <- 1000

# set coefficients
true_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
biased_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
iv_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)

set.seed(456) 

for (i in 1:sim) {
  # generate correlated variables xR and c
  xR_and_c <- mvrnorm(1000, c(2, 1.5), matrix(c(1, 0.55, 0.55, 1), 2, 2))
  xR <- xR_and_c[, 1]
  c <- xR_and_c[, 2]  # unobserved confounder
  
  # instrument
  z <- rnorm(1000)
  
  # observed x
  x <- xR + z
  
  # outcome
  y <- 1 + x + c + rnorm(1000, 0, 0.5)
  
  # true model (not observable in practice because c is unobserved)
  true_model <- lm(y ~ x + c)
  true_model_coeffs[i, ] <- coef(true_model)[2:3]  # Store coefficients of x and c
  
  # biased model (no correction for c)
  biased_model <- lm(y ~ x)
  biased_model_coeffs[i, ] <- coef(biased_model)[1:2] # store intercept and beta
  
  # IV approach
  x_pred <- lm(x ~ z)$fitted.values
  iv_model <- lm(y ~ x_pred)
  iv_model_coeffs[i, ] <- coef(iv_model)[1:2]  # Store intercept and coefficient for x_pred
}

# create dfs
true_model_results <- data.frame(
  Simulation = 1:sim,
  Beta_X_True = true_model_coeffs[, 1],
  Beta_C_True = true_model_coeffs[, 2]
)

biased_model_results <- data.frame(
  Simulation = 1:sim,
  Intercept_IV = biased_model_coeffs[, 1],
  Beta_X_IV = biased_model_coeffs[, 2]
)

iv_model_results <- data.frame(
  Simulation = 1:sim,
  Intercept_IV = iv_model_coeffs[, 1],
  Beta_X_IV = iv_model_coeffs[, 2]
)

# visualize unbiasedness of iv approach: histogram of x in IV model 
iv_x_mean <- mean(iv_model_results$Beta_X_IV)
true_x_mean <- mean(true_model_results$Beta_X_True)

hist_coef <- ggplot(iv_model_results, aes(x = Beta_X_IV)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = iv_x_mean, color = "blue", linetype = "solid", size = 1.0, 
             aes(label = paste("Mean IV Estimate: ", round(iv_x_mean, 2)))) +
  geom_vline(xintercept = true_x_mean, color = "red", linetype = "dashed", size = 1.2, 
             aes(label = paste("Mean True Estimate: ", round(true_x_mean, 2)))) +
  labs(title = "Distribution of IV Model Estimates of X", x = "Beta_X_IV", y = "Frequency")

hist_coef
