# IV simulation 

library(MASS)
library(ggplot2)


### set up simulation parameters and empty storage matrices
sim <- 1000
N <- 100 # sample size
# set coefficients
true_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
biased_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
iv_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)

# simulate dataset 
set.seed(456) 
for (i in 1:sim) {
  # generate correlated variables xR and c
  xR_and_c <- mvrnorm(N, c(2, 1.5), matrix(c(1, 0.55, 0.55, 1), 2, 2))
  xR <- xR_and_c[, 1]
  c <- xR_and_c[, 2]  # unobserved confounder
  
  # instrument
  z <- rnorm(N)
  
  # observed x
  x <- xR + z  # should be binary
  
  # potential outcomes
  y0 <- 1 + 1.5*0 + c + rnorm(N, 0, 0.5)
  y1 <- 1 + 1.5*1 + c + rnorm(N, 0, 0.5)  # real effect of 1.5
  
  # outcome Y based on treatment X
  y <- x * y1 + (1-x) * y0
  # y <- 1 + 1.5*x + c + rnorm(N, 0, 0.5)
  
  # dataset
  df <- data.frame(x, y, y1, y0, z)
  
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

  # empirical ate
  ate_emp <- mean(y[x == 1]) - mean(y[x == 0])
  }

# create dfs
true_model_results <- data.frame(
  Simulation = 1:sim,
  ATE_emp = true_model_coeffs[, 1],  # estimated unbiased ATE
  Beta_C_True = true_model_coeffs[, 2],  # 
  ATE_true = mean(df$y1) - mean(df$y0)  # true ATE
)

true_ATE_emp <- mean(true_model_results$ATE_emp)

biased_model_results <- data.frame(
  Simulation = 1:sim,
  Intercept_IV = biased_model_coeffs[, 1],
  ATE_emp = biased_model_coeffs[, 2]  # biased ate
  #ATE_emp2 = ate_emp
)

biased_ATE_emp <- mean(biased_model_results$ATE_emp)

iv_model_results <- data.frame(
  Simulation = 1:sim,
  Intercept_IV = iv_model_coeffs[, 1],
  ATE_emp = iv_model_coeffs[, 2]  # unbiased ate
  #ATE = ate_emp
)

iv_ATE_emp <- mean(iv_model_results$ATE_emp)

hist(iv_model_results$ATE_emp)
abline(v=true_ATE_emp, col = "red")
abline(v=biased_ATE_emp, col = "green")  
abline(v=iv_ATE_emp, col = "purple")


iv_model_histogram <- ggplot(iv_model_results, aes(x = ATE_emp)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = true_ATE_emp, color = "True ATE"), linetype = "dashed", size = 1.0) +
  geom_vline(aes(xintercept = biased_ATE_emp, color = "ATE (biased model)"), linetype = "solid", size = 1.0) +
  geom_vline(aes(xintercept = iv_ATE_emp, color = "ATE (IV model)"), linetype = "solid", size = 1.0) +
  scale_color_manual(
    name = NULL, # Remove legend title
    values = c("True ATE" = "red", "ATE (biased model)" = "green", "ATE (IV model)" = "purple"),
    breaks = c("True ATE", "ATE (biased model)", "ATE (IV model)"), # Ensure order of legend items
    labels = c("True ATE", "ATE (biased model)", "ATE (IV model)") # Custom labels
  ) +
  labs(
    title = "Distribution of ATE Estimates for IV Model",
    x = "Estimated ATE",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = c(0.85, 0.85), # Place legend inside the grid
    legend.text = element_text(size = 9), # Adjust legend text size
    legend.background = element_blank() # Transparent background
  ) +
  guides(color = guide_legend(
    override.aes = list(
      linetype = c("solid", "solid", "solid"), 
      size = 0.5
    )
  ))

ggsave("iv_hist_ate.png", iv_model_histogram, width = 8, height = 6)
print(iv_model_histogram)


# visualize unbiasedness of iv approach: histogram of x in IV model 
iv_x_mean <- mean(iv_model_results$ATE_emp)
true_x_mean <- mean(true_model_results$ATE_true)

hist_coef <- ggplot(iv_model_results, aes(x = ATE_emp)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = iv_x_mean, color = "blue", linetype = "solid", size = 1.0) +
  geom_vline(xintercept = true_x_mean, color = "red", linetype = "dashed", size = 1.2) +
  labs(
    title = "Distribution of IV Model Estimates of X",
    x = "Coefficient Estimate",
    y = "Frequency",
    caption = "Blue line: Mean IV Estimate; Red dashed line: Mean True Estimate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

ggsave("iv_model_histogram.png", width = 8, height = 6)
hist_coef
