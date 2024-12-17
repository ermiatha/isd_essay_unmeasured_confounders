# Simulation Study
# 15.12.2024
# Ermioni Athanasiadi
# Generate Data for sensitivity analysis with e-value

library(tidyverse)
library(ggplot2)


# Variant 1: with binary confounder #####
set.seed(456)

sim <- 100  # number of simulations
N <- 100  # sample size per simulation

# Coefficients
beta_ux <- c(0.5, 0.8, 1.8, 3.5)  # Coefficient for binary confounder (U) on X
#beta_ux <- 0.8
beta_y0 <- 1
beta_y1 <- 1.5

# prob_u <- c(0.5, 0.6, 0.75, 0.9)
prob_u <- 0.5

m <- length(beta_ux)

# create a list to store all dataframes of varying m
results_list <- vector("list", m)

results <- data.frame(
  sim = integer(sim),
  j = integer(sim),
  beta_ux = integer(sim),
  coef_x = numeric(sim),
  coef_x_u = numeric(sim),
  true_ATE = numeric(sim),
  estim_ATE = numeric(sim),
  rr = numeric(sim),
  e_value = numeric(sim)
)


results_by_u <- data.frame(
  u_variant = integer(m),
  mean_evalue = numeric(m),
  true_ATE = numeric(m),
  estim_ATE = numeric(m),
  bias = numeric(m)
                            )

for (j in 1:m) {
  for (i in 1:sim) {
    # Unmeasured confounder U
    u <- rbinom(N, 1, prob_u)
    
    # treatment X based on U
    prob_x <- plogis(beta_ux[j] * u)
    x <- rbinom(N, 1, prob_x)
    
    error <- rnorm(N)
    
    # potential outcomes
    y0 <- rbinom(N, 1, prob = plogis(beta_y0 * u))
    y1 <- rbinom(N, 1, prob = plogis(beta_y1 * u))
    
    # Binary outcome Y based on X, U
    #  y_prob <- plogis(beta0 + beta1 * x + beta2 * u + error)
    y <- x * y1 + (1-x) * y0
    
    df <- data.frame(id = 1:N, x, y, y1, y0, u)
    
    
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
    #assign(paste0("results", j), data.frame(
      sim = i,
      j = j,
      beta_ux = beta_ux[j],
      coef_x = coef(m1)["x"],  # Coefficient for X without U
      coef_x_u = coef(m2)["x"],  # Coefficient for X with U
      true_ATE =  mean(df$y1) - mean(df$y0),
      estim_ATE = mean(df$y[df$x == 1]) - mean(df$y[df$x == 0]),
      rr = RR,  # Relative risk
      e_value = RR + sqrt(RR * (RR-1))

      )
  
  }
  results_list[[j]] <- results
  
  results_by_u[j, ] <- c(
    u_variant = j,
    mean_evalue = mean(results$e_value, na.rm = T),
    true_ATE = mean(results$true_ATE),
    estim_ATE = mean(results$estim_ATE),
    bias = 0
                        )
}

tot_results <- do.call(rbind, results_list)

# calculate bias as difference between real and estimated ATE
results_by_u$bias = results_by_u$estim_ATE - results_by_u$true_ATE
# add also diff between RR's ?

# look at results
results_by_u
summary(results_by_u)


results_by_u$u_variant <- as.factor(results_by_u$u_variant)


long_totres <- tot_results %>%
  pivot_longer(
    names_to = "ATE",
    values_to = "ATE_values",
    cols = c("true_ATE",  "estim_ATE")
    
  )

str(long_totres)
long_totres$sim <-  as.factor(long_totres$sim)
long_totres$j <-    as.factor(long_totres$j)

# Visualize results

ggplot(tot_results, aes(x = factor(j), y = e_value)) +
  geom_boxplot()


ggplot(long_totres, aes(x = factor(j), y = ATE_values, fill = ATE)) +
  geom_boxplot()

## Variant 2: with continuous confounder ######################################

set.seed(456)

sim <- 200 # number of simulations
N <- 100  # sample size per simulation

# Coefficients
beta_ux <- c(0.5, 0.8, 1.8, 3.5)  # Coefficient for binary confounder (U) on X
#beta_ux <- 0.8
beta_y0 <- 1
beta_y1 <- 2.5

# mu <- c(0.5, 1.5, 2.5, 4)
# mu <- 1.5
mu <- c(-10, -5, -1, -0.5, -0.2, -0.1, 0, 0.1, 0.2, 0.5, 1, 2, 3, 4, 5)

m <- length(mu)


# create a list to store all dataframes of varying m
results_list <- vector("list", m)

results <- data.frame(
  sim = integer(sim),
  j = integer(sim),
  beta_ux = integer(sim),
  coef_x = numeric(sim),
  coef_x_u = numeric(sim),
  true_ATE = numeric(sim),
  estim_ATE = numeric(sim),
  rr = numeric(sim),
  e_value = numeric(sim)
)


results_by_u <- data.frame(
  u_variant = integer(m),
  mean_evalue = numeric(m),
  true_ATE = numeric(m),
  estim_ATE = numeric(m),
  bias = numeric(m)
)

for (j in 1:m) {
  for (i in 1:sim) {
    # Unmeasured confounder U
    u <- rnorm(N, mean = mu[j], sd = 1)
    
    # treatment X based on U
    prob_x <- plogis(beta_ux * u)  # p = 1/(1+exp(-x))
    x <- rbinom(N, 1, prob_x)
    
    error <- rnorm(N)
    
    # potential outcomes
    y0 <- rbinom(N, 1, prob = plogis(beta_y0 * u))
    y1 <- rbinom(N, 1, prob = plogis(beta_y1 * u))
    
    # Binary outcome Y based on X, confounded with U
    #  y_prob <- plogis(beta0 + beta1 * x + beta2 * u + error)
    y <- x * y1 + (1-x) * y0
    
    df <- data.frame(id = 1:N, x, y, y1, y0, u)
    
    
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
      #assign(paste0("results", j), data.frame(
      sim = i,
      j = j,
      beta_ux = beta_ux[j],
      coef_x = coef(m1)["x"],  # Coefficient for X without U
      coef_x_u = coef(m2)["x"],  # Coefficient for X with U
      true_ATE =  mean(df$y1) - mean(df$y0),
      estim_ATE = mean(df$y[df$x == 1]) - mean(df$y[df$x == 0]),
      rr = RR,  # Relative risk
      e_value = RR + sqrt(RR * (RR-1))
      
    )
    
  }
  results_list[[j]] <- results
  
  results_by_u[j, ] <- c(
    u_variant = j,
    mean_evalue = mean(results$e_value, na.rm = T),
    true_ATE = mean(results$true_ATE),
    estim_ATE = mean(results$estim_ATE),
    bias = 0
  )
}

tot_results <- do.call(rbind, results_list)

# calculate bias as difference between real and estimated ATE
results_by_u$bias = results_by_u$estim_ATE - results_by_u$true_ATE
# add also diff between RR's ?

# look at results
results_by_u
summary(results_by_u)


results_by_u$u_variant <- as.factor(results_by_u$u_variant)


long_totres <- tot_results %>%
  pivot_longer(
    names_to = "ATE",
    values_to = "ATE_values",
    cols = c("true_ATE",  "estim_ATE")
    
  )

str(long_totres)
long_totres$sim <-  as.factor(long_totres$sim)
long_totres$j <-    as.factor(long_totres$j)

# Visualize results

