# BDA400 Assignment 5 - Linear Regression (linreg)
# Student: Parvana Babali
# Base R only - the regression parameters are calculated directly.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource)) {
    stop("regressionSource must be a numeric vector")
  }
  if (anyNA(regressionSource)) stop("regressionSource must not contain NA values")

  n <- length(regressionSource)

  if (length(regressionLength) != 1 || !is.numeric(regressionLength) ||
      is.na(regressionLength) || regressionLength <= 1 ||
      regressionLength != as.integer(regressionLength)) {
    stop("regressionLength must be an integer greater than 1")
  }
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (length(regressionOffset) != 1 || !is.numeric(regressionOffset) ||
      is.na(regressionOffset) || regressionOffset < 0 ||
      regressionOffset != as.integer(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  if (regressionOffset + regressionLength > n) {
    stop("regressionLength plus regressionOffset exceeds the available data")
  }

  # In R's one-based indexing, +1 ensures exactly regressionLength values.
  start_index <- n - regressionOffset - regressionLength + 1
  end_index <- n - regressionOffset
  source_subset <- regressionSource[start_index:end_index]

  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)

  numerator <- sum((index_values - mean_index) *
                     (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  if (denominator == 0) stop("cannot calculate regression with zero index variance")

  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept

  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )

  return(result)
}
