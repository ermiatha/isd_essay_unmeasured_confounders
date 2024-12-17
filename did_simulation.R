# 2x2 DiD simulation
set.seed(11235)

# Simulate data
n <- 100
time <- rep(c(0, 1), times = n)
group <- rep(c(0, 1), each = 2 * n / 2)

# Unmeasured confounder (constant over time)
u <- rnorm(n * 2, mean = 2, sd = 1)

# Base outcome: includes the unmeasured confounder
base_outcome <- 5 + 0.5 * u + rnorm(n * 2)

# Time effect (same for both groups)
time_effect <- 0.5 * time

# Treatment effect (ATT)
treatment_effect <- 2
treatment_effect_applied <- treatment_effect * (group == 1 & time == 1)

# Outcome variable
y <- base_outcome + time_effect + treatment_effect_applied

# Create a data frame
data <- data.frame(id = 1:(n * 2), time, group, y)

# Fit the Difference-in-Differences (DiD) model
did_model <- lm(y ~ group * time, data = data)

# Extract the DiD estimate (interaction term)
did_estimate <- coef(did_model)["group:time"]

# Display results
cat("True ATT:", treatment_effect, "\n")
cat("Estimated ATT (DiD):", did_estimate, "\n")
summary(did_model)

# Is DiD estimator truly unbiased? Simulation over 1000 experiments

# Load necessary libraries

# Parameters
n <- 100
num_simulations <- 1000
treatment_effect <- 2 # True ATT

# Function to simulate one dataset and estimate the DiD coefficient
simulate_did <- function() {
  
  # Same code as before
  time <- rep(c(0, 1), times = n)
  group <- rep(c(0, 1), each = n)
  u <- rnorm(n * 2, mean = 2, sd = 1)
  base_outcome <- 5 + 0.5 * u + rnorm(n * 2)
  time_effect <- 0.5 * time
  treatment_effect_applied <- treatment_effect * (group == 1 & time == 1)
  y <- base_outcome + time_effect + treatment_effect_applied
  data <- data.frame(id = 1:(n * 2), time, group, y)
  did_model <- lm(y ~ group * time, data = data)
  did_estimate <- coef(did_model)["group:time"]
  return(did_estimate)
}
# Run the simulation 1,000 times
did_estimates <- replicate(num_simulations, simulate_did())

# Compute the average DiD estimate and its standard deviation
average_did_estimate <- mean(did_estimates)
std_dev_did_estimate <- sd(did_estimates)
cat("True ATT:", treatment_effect, "\n")
cat("Average Estimated ATT (DiD) over", num_simulations, "simulations:", average_did_estimate, "\n")
cat("Standard Deviation of DiD Estimates:", std_dev_did_estimate, "\n")

# Plot the histogram of DiD estimates
hist(did_estimates, 
     breaks = 30, 
     col = "skyblue", 
     main = "Distribution of DiD Estimates",
     xlab = "DiD Estimate", 
     border = "white")
abline(v = treatment_effect, col = "red", lwd = 2, lty = 2)
abline(v = average_did_estimate, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("True ATT", "Average DiD Estimate"),
       col = c("red", "blue"), lty = 2, lwd = 2, bty = "n")