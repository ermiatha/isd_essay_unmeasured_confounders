# Simulation Study
# 15.12.2024
# Ermioni Athanasiadi

library(tidyverse)
library(ggplot2)


# Step 1: Generate Data for sensitivity analysis with e-value

set.seed(456)

sim <- 1000  # number of simulations
N <- 100  # sample size per simulation

# Coefficients
beta0 <- 0.1
beta1 <- 0.7  # Coefficient for treatment (X)
beta_ux <- 0.8  # Coefficient for confounder (U) on X
beta2 <- 1.9  # Coefficient for confounder (U) on Y
beta3 <- 0.9

results <- data.frame(
  sim = integer(sim),
  coef_x = numeric(sim),
  coef_x_u = numeric(sim),
  rr = numeric(sim),
  e_value = numeric(sim)
)

for (i in 1:sim) {
  # Unmeasured confounder U
  u <- rbinom(N, 1, 0.5)
  
  # treatment X based on U
  prob_x <- plogis(beta_ux * u)
  x <- rbinom(N, 1, prob_x)
  
  error <- rnorm(N)
  
  # potential outcomes
  y0 <- rbinom(N, 1, prob = plogis(beta2 * u))
  y1 <- rbinom(N, 1, prob = plogis(beta2 * u + beta3))
  
  # Binary outcome Y based on X, U
 #  y_prob <- plogis(beta0 + beta1 * x + beta2 * u + error)
  y <- x * y1 + (1-x) * y0
  
  df <- data.frame(id = 1:N, x, y, y1, y0, u)
  
  trueATE <- mean(df$y1) - mean(df$y0)
  
  m1 <- glm(y ~ x, data = df, family = "binomial")  # Without U
  m2 <- glm(y ~ x + u, data = df, family = "binomial")  # With U
  
  cont_tab <- table(df$y, df$x)
  A <- cont_tab[1]
  B <- cont_tab[3]
  C <- cont_tab[2]
  D <- cont_tab[4]
  
  # Compute relative risk (RR)
  RR <- (A / (A + B)) / (C / (C + D))
  # RR for E-value: assumed to be equivalent between U = 0 and U = 1
  
  # results
  results[i, ] <- c(
    sim = i,
    coef_x = coef(m1)["x"],  # Coefficient for X without U
    coef_x_u = coef(m2)["x"],  # Coefficient for X with U
    rr = RR,  # Relative risk
    e_value = RR + sqrt(RR * (RR-1))
  )
}

# look at results
summary(results)
hist(results$e_value)


# Average estimates
mean_coef_x <- mean(results$coef_x)
mean_coef_x_u <- mean(results$coef_x_u)
mean_rr <- mean(results$rr)
mean_e_value <- mean(results$e_value, na.rm = T)  # 18 NAs, negative sqrt

# ATE for binary data?
## get mean probability diff between x=1 and x=0

### Determine what to vary
# strength of confounder
# type of confounder: binary, categorical, continuous
# sample size


# Add measured covariates 
gender <- rbinom(N, 1, 0.55)
age <- sample(18:55, N, replace = T)
edu <- rbinom(N, 1, 0.5)


# Step 2: Account for unmeasured confounding using IV (instrumental variable)

# Dataset with variable correlated with X but not y
iv <- rbinom(N, 1, prob_x)  
# 3 step aproach, regression, prediction, regression


# Step 3: Account for unmeasured confounding using DiD
# repeated measured data structure
# continuous outcome

df$post <- 0
# df <- df %>% mutate(id=row_number())
df2 <- df %>% mutate(post=1)
df <- rbind(df, df2)



