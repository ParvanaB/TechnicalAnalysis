# BDA400 Assignment 5 - Standard Deviation (stdev)
# Student: Parvana Babali
# Implements the population standard deviation formula from the assignment.

stdev <- function(data) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(data) == 0) stop("data must contain at least one value")
  if (anyNA(data)) stop("data must not contain NA values")

  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
