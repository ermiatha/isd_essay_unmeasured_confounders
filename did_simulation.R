# Title: Inference for Statistics and Data Science - Paper - DiD Simulation
# Author: Matteo Ramina
# Group: 2
# Date: 19/12/2024

#------------------------------------------------------------------------------
# 1 Setup

library(stargazer)
library(showtext)
set.seed(11235)

#------------------------------------------------------------------------------
# 2 Data generation

n <- 250  # Half sample size
a <- rep(2, times = n)  # Intercept
t <- rep(c(0, 1), times = n) # t indicator
g <- rep(c(0, 1), each = 2 * n / 2) # g indicator
x_1 <- rnorm(n * 2, mean = 1 + 0.5 * g, sd = 1) # Measured confounder
u_1 <- 0.5 * t # Unmeasured time effect (same for both groups)
u_2 <- rnorm(n * 2, mean = 3 + 0.5 * g, sd = 1) # Time-invariant unmeasured confounder (different across groups)
att <- 2 # Treatment effect (ATT)
d <- g == 1 & t == 1 # Treatment indicator
# Error term
e <- rnorm(n * 2)
y <- a + x_1 + u_1 + u_2 + att * d + e # Outcome variable

# Create a data frame
data <- data.frame(id = 1:(n * 2), y, x_1, t, g, d)

#------------------------------------------------------------------------------
# 3 First DiD simulation

# Fit the DiD model with the measured confounder
did_model_1 <- lm(y ~ g + t + d, data = data)
did_model_2 <- lm(y ~ g + t + d + x_1, data = data)

# Extract the DiD estimate and display and extract results
summary(did_model_1)
summary(did_model_2)

stargazer(did_model_1, did_model_2, type = "latex",
          title = "Difference-in-Differences Models",
          dep.var.labels = c("Outcome Variable"),
          covariate.labels = c("Group", "Time", "Treatment", "Measured Confounder"),
          out = "did_models.tex")

#------------------------------------------------------------------------------
# 4 Second DiD simulation

# Function to simulate one dataset and estimate the DiD coefficient
simulate_did <- function() {
  # Same code as before
  t <- rep(c(0, 1), times = n)
  g <- rep(c(0, 1), each = 2 * n / 2)
  x_1 <- rnorm(n * 2, mean = 1 + 0.5 * g, sd = 1)
  u_2 <- rnorm(n * 2, mean = 3 + 0.5 * g, sd = 1)
  e <- rnorm(n * 2)
  u_1 <- 0.5 * t
  d <- g == 1 & t == 1
  att <- 2
  y <- x_1 + u_2 + u_1 + att * d + e
  data <- data.frame(id = 1:(n * 2), t, g, x_1, y)
  did_model <- lm(y ~ g * t + x_1, data = data)
  did_estimate <- coef(did_model)["g:t"]
  return(did_estimate)
}

# Run the simulation 1,000 times
num_simulations = 1000
did_estimates <- replicate(num_simulations, simulate_did())

# Compute the average DiD estimate and its standard deviation
did_estimate_avg <- mean(did_estimates)
did_estimate_std <- sd(did_estimates)
cat("True ATT:", att, "\n")
cat("Average Estimated ATT (DiD) over", num_simulations, "simulations:", did_estimate_avg, "\n")
cat("Standard Deviation of DiD Estimates:", did_estimate_std, "\n")

# Plot the histogram of DiD estimates
png("shared_outputs/did_estimates_histogram.png", width = 2400, height = 1800, res = 300)
font_add("LMRoman12", "lmroman12-regular.otf")
showtext_auto()
par(family = "LMRoman12")
hist(did_estimates, 
     breaks = 30, 
     col = "skyblue", 
     main = "",
     xlab = "DiD Estimate", 
     border = "white", 
     family = "LMRoman12",
     xlim = c(1, 3),
     ylim = c(0, 100))
abline(v = att, col = "red", lwd = 2, lty = 2)
abline(v = did_estimate_avg, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("True ATT", "Average DiD Estimate"),
       col = c("red", "blue"), lty = 2, lwd = 2, bty = "n")
dev.off()

#------------------------------------------------------------------------------
# END