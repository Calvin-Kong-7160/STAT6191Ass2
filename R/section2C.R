# Section 2 Part C: Profile likelihood for beta1

# Use the MLE values from optim()
beta0_hat <- optim_coef["beta0_intercept"]
beta1_hat <- optim_coef["beta1_log_gdp"]

beta0_hat
beta1_hat

# DEfine Beta1
beta1_grid <- seq(
  from = beta1_hat - 1,
  to = beta1_hat + 1,
  length.out = 200
)

# Compute log-likelihood value
profile_loglik <- sapply(beta1_grid, function(beta1_value) {
  log_likelihood(
    beta = c(beta0_hat, beta1_value),
    data = data
  )
})

profile_df <- data.frame(
  beta1 = beta1_grid,
  log_likelihood = profile_loglik
)

head(profile_df)


# Plot likelihood curve
library(ggplot2)

ggplot(profile_df, aes(x = beta1, y = log_likelihood)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = beta1_hat, linetype = "dashed") +
  labs(
    title = "Profile Log-Likelihood for beta1",
    subtitle = "Coefficient of log(GDP per capita)",
    x = expression(beta[1]),
    y = "Profile log-likelihood"
  ) +
  theme_minimal()
