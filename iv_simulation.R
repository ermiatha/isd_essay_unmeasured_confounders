# IV simulation 

library(MASS)
library(ggplot2)
library(showtext)


### set up simulation parameters and empty storage matrices
sim <- 500
N <- 100 # sample size
# set coefficients
true_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
biased_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)
iv_model_coeffs <- matrix(NA, nrow = sim, ncol = 2)

beta <- 0.5 # control z with coefficient
beta_c <- c(0.8, 1.0, 1.2, 1.5)  # varying probabilities for c
m <- length(beta_c)


# simulate dataset 
set.seed(456)
for (j in 1:m) {
  for (i in 1:sim) {
  
    # generate variables z, xR and c
    z <- rnorm(N, mean = 0, sd = 1) # instrument

    prob_z <- plogis(beta * z)  # Probabilities depend on x
   
    c <- rnorm(N, mean = beta_c[j], sd = 0.5) # Unmeasured confounder c
    prob_c <- plogis(c)
    prob_xR <- plogis(0.5 * prob_c)  # generate x based on probability of c
    xR <- rbinom(N, 1, prob_xR)
    
    # generate x based on z and xR
    x <- rbinom(N, size = 1, prob = prob_z * prob_xR)
    cor(z, x)  # check empirical correlation
    
    # potential outcomes
    y0 <- 1 + 1.5*0 + c + rnorm(N, 0, 0.5)
    y1 <- 1 + 1.5*1 + c + rnorm(N, 0, 0.5)  # real effect of 1.5
    
    # outcome Y based on treatment X
    y <- x * y1 + (1-x) * y0
    # y <- 1 + 1.5*x + c + rnorm(N, 0, 0.5)
    
    # dataset
    df <- data.frame(xR, x, y, y1, y0, z)
    
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

font_add("LMRoman12", "lmroman12-regular.otf")
showtext_auto()
png("shared_outputs/iv_hist_ate.png", width = 1200, height = 900, res = 300)
par(family = "LMRoman12")
hist(iv_model_results$ATE_emp, 
     breaks = 30,
     col = "skyblue", 
     border = "white",
     main = "",
     xlab = "Estimated ATE", 
     ylab = "Frequency", 
     xlim = c(0.5, 2.5),
     ylim = c(0, 120))
abline(v = true_ATE_emp, col = "red", lwd = 2, lty = 2)
abline(v = biased_ATE_emp, col = "green", lwd = 2, lty = 1)
abline(v = iv_ATE_emp, col = "purple", lwd = 2, lty = 1)
legend("topright", 
       legend = c("True ATE", "ATE (biased model)", "ATE (IV model)"), 
       col = c("red", "green", "purple"), 
       lty = c(2, 1, 1), 
       lwd = 2, 
       bty = "n", 
       cex = 1)
dev.off()

'
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
'