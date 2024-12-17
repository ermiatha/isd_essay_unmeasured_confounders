## Simulate data with Potential Outcomes in RCT Trial

simulate_rct <- function(N, beta0, beta1, sigma, p_treatment, seed) {
  set.seed(seed) # Set seed for reproducibility
  
  # Generate treatment assignment
  A <- rbinom(N, size = 1, prob = p_treatment)
  
  # Potential outcomes
  Y0 <- rnorm(N, mean = beta0, sd = sigma)         # Control group outcome
  Y1 <- rnorm(N, mean = beta0 + beta1, sd = sigma) # Treatment group outcome
  
  # Observed outcome
  Y <- A * Y1 + (1 - A) * Y0
  
  # Create and return data frame
  data <- tibble::tibble(A = A, Y0 = Y0, Y1 = Y1, Y = Y)
  return(data)
}

result <- simulate_rct(N = 500, beta0 = 10, beta1 = 2, sigma = 1, p_treatment = 0.5, seed = 42)
print(result)

true_ate <- function(data) {
  mean(data$Y1 - data$Y0)
}


true_ate(
  simulate_rct(N = 500, beta0 = 10, beta1 = 2, sigma = 1, p_treatment = 0.5, seed = 42)
)  # 2.05


obs_ate <- function(data) {
  mean(data$Y[data$A == 1]) - mean(data$Y[data$A == 0])
}

obs_ate(
  simulate_rct(N = 500, beta0 = 10, beta1 = 2, sigma = 1, p_treatment = 0.5, seed = 42)
)  # 2.07

## estimated ATE is unbiased



####### the same with binaries
simulate_rct <- function(N, p_contr, p_treat, p_treat_assign, seed) {
  set.seed(seed) # Set seed for reproducibility
  
  # Generate treatment assignment
  A <- rbinom(N, size = 1, prob = p_treat_assign)
  
  # Potential outcomes
  Y0 <- rbinom(N, 1, plogis(p_contr))         # Control group outcome
  Y1 <- rbinom(N, 1, plogis(p_treat)) # Treatment group outcome


  # Observed outcome
  Y <- A * Y1 + (1 - A) * Y0
  
  # Create and return data frame
  data <- tibble::tibble(A = A, Y0 = Y0, Y1 = Y1, Y = Y)
  return(data)
}

result <- simulate_rct(N = 1000, p_contr = 0.3, p_treat = 0.6, p_treat_assign = 0.5, seed = 42)
print(result)

true_ate <- function(data) {
  mean(data$Y1 - data$Y0)
}


true_ate(
  simulate_rct(N = 1000, p_contr = 0.3, p_treat = 0.6, p_treat_assign = 0.5, seed = 42)
)  # 0.06


obs_ate <- function(data) {
  mean(data$Y[data$A == 1]) - mean(data$Y[data$A == 0])
}

obs_ate(
  simulate_rct(N = 1000, p_contr = 0.3, p_treat = 0.6, p_treat_assign = 0.5, seed = 42)
)  # 0.065

## estimated ATE is unbiased