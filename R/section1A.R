# Section 1 Part A: Clean exponential data

library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(270)

# Simulation settings
n_samples <- 1000
n <- 250
lambda <- 1
true_mean <- 1 / lambda

## Section 1A:
# Generate 1000 samples and calculate estimators
clean_results <- replicate(n_samples, {
  sample_data <- rexp(n, rate = lambda)

  c(
    mean = mean(sample_data),
    median = median(sample_data),
    trimmed_mean = mean(sample_data, trim = 0.10)
  )
})

# Convert to data frame
clean_results <- as.data.frame(t(clean_results))

head(clean_results)

## Section 1B
clean_summary <- clean_results %>%
  pivot_longer(
    cols = everything(),
    names_to = "estimator",
    values_to = "estimate"
  ) %>%
  group_by(estimator) %>%
  summarise(
    mean_estimate = mean(estimate),
    bias = mean(estimate) - true_mean,
    MCSD = sd(estimate),
    MSE = mean((estimate - true_mean)^2),
    .groups = "drop"
  )

clean_summary

## Section 1C
clean_results_long <- clean_results %>%
  pivot_longer(
    cols = everything(),
    names_to = "estimator",
    values_to = "estimate"
  )

ggplot(clean_results_long, aes(x = estimate)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~ estimator, scales = "free") +
  labs(
    title = "Sampling Distributions of Estimators: Clean Data",
    x = "Estimate",
    y = "Frequency"
  ) +
  theme_minimal()


# Part A: Compute Bias, MCSD, and MSE

true_mean <- 1

partA_summary <- clean_results %>%
  pivot_longer(
    cols = everything(),
    names_to = "Estimator",
    values_to = "Estimate"
  ) %>%
  group_by(Estimator) %>%
  summarise(
    Mean_of_Estimates = mean(Estimate),
    Bias = Mean_of_Estimates - true_mean,
    MCSD = sd(Estimate),
    MSE = mean((Estimate - true_mean)^2),
    .groups = "drop"
  )

partA_summary
