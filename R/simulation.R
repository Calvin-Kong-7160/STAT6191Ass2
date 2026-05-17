#' Summarise Estimator Performance
#'
#' Calculates mean estimate, bias, Monte Carlo standard deviation, and MSE.
#'
#' @param estimates A matrix of simulated estimator values.
#' @param true_value The true population value used for comparison.
#'
#' @return A data frame containing estimator performance statistics.
#' @export
summarise_performance <- function(estimates, true_value) {
  data.frame(
    estimator = rownames(estimates),
    mean_estimate = rowMeans(estimates),
    bias = rowMeans(estimates) - true_value,
    mcsd = apply(estimates, 1, sd),
    mse = rowMeans((estimates - true_value)^2)
  )
}


#' Run Clean Completion Time Simulation
#'
#' Simulates clean completion times from an exponential distribution.
#'
#' @param n Sample size.
#' @param reps Number of simulation replications.
#' @param rate Rate parameter for the exponential distribution.
#'
#' @return A data frame comparing estimator performance.
#' @export
simulate_clean <- function(n = 250, reps = 1000, rate = 1) {
  true_mean <- 1 / rate

  estimates <- replicate(reps, {
    x <- rexp(n, rate = rate)
    calculate_estimators(x)
  })

  summarise_performance(estimates, true_mean)
}


#' Run Contaminated Completion Time Simulation
#'
#' Simulates completion times with 90% clean observations and 10% contaminated observations.
#'
#' @param n Sample size.
#' @param reps Number of simulation replications.
#' @param contamination_prop Proportion of contaminated observations.
#'
#' @return A data frame comparing estimator performance.
#' @export
simulate_contaminated <- function(n = 250, reps = 1000, contamination_prop = 0.10) {
  n_contaminated <- round(n * contamination_prop)
  n_clean <- n - n_contaminated

  true_mean <- (1 - contamination_prop) * 1 + contamination_prop * 50

  estimates <- replicate(reps, {
    x <- c(
      rexp(n_clean, rate = 1),
      rexp(n_contaminated, rate = 0.02)
    )

    calculate_estimators(x)
  })

  summarise_performance(estimates, true_mean)
}
