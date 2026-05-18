# Section 2 Part B: Fisher Information and Confidence Intervals

# 1. Extract the Hessian matrix from optim()
hessian_matrix <- optim_output$hessian

hessian_matrix

# 2. Fisher information matrix
fisher_information <- hessian_matrix

fisher_information

# 3. Variance-covariance matrix
vcov_matrix <- solve(fisher_information)

vcov_matrix

# 4. Standard errors
standard_errors <- sqrt(diag(vcov_matrix))
names(standard_errors) <- c("beta0_intercept", "beta1_log_gdp")
standard_errors

# 5. Manual 95% confidence intervals

z_value <- 1.96

ci_lower <- optim_coef - z_value * standard_errors
ci_upper <- optim_coef + z_value * standard_errors
confidence_intervals <- data.frame(
  Parameter = c("Intercept", "log_gdp"),
  Estimate = optim_coef,
  Standard_Error = standard_errors,
  CI_Lower_95 = ci_lower,
  CI_Upper_95 = ci_upper
)
confidence_intervals

partB_summary <- data.frame(
  Parameter = c("Intercept", "log_gdp"),
  Estimate = round(optim_coef, 4),
  Standard_Error = round(standard_errors, 4),
  CI_Lower_95 = round(ci_lower, 4),
  CI_Upper_95 = round(ci_upper, 4)
)

partB_summary
