#' Calculate Completion Time Estimators
#'
#' Calculates the sample mean, median, and 10% trimmed mean.
#'
#' @param x A numeric vector of completion times.
#'
#' @return A named numeric vector containing the mean, median, and trimmed mean.
#' @export
calculate_estimators <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  c(
    mean = mean(x),
    median = median(x),
    trimmed_mean = mean(x, trim = 0.10)
  )
}
