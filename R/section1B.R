# Section 1 Part B: Contaminated exponential data

library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(270)

# Simulation settings
n_samples <- 1000
n <- 250

# Contamination settings
main_rate <- 1
outlier_rate <- 0.02
contamination_prob <- 0.10

# True mean of the contaminated distribution
true_contaminated_mean <- 0.90 * (1 / main_rate) +
  0.10 * (1 / outlier_rate)

true_contaminated_mean


contaminated_results <- replicate(n_samples, {

  # Decide whether each observation comes from normal or contaminated group
  is_outlier <- rbinom(n, size = 1, prob = contamination_prob)

  # Generate observations
  sample_data <- ifelse(
    is_outlier == 1,
    rexp(n, rate = outlier_rate),
    rexp(n, rate = main_rate)
  )

  # Calculate estimators
  c(
    mean = mean(sample_data),
    median = median(sample_data),
    trimmed_mean = mean(sample_data, trim = 0.10)
  )
})

# Convert to data frame
contaminated_results <- as.data.frame(t(contaminated_results))

head(contaminated_results)
