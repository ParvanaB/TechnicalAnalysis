# BDA400 Assignment 5 - Simple Moving Average (SMA)
# Student: Parvana Babali
# Base R only - no indicator libraries are used.

sma <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period <= 0 || period != as.integer(period)) {
    stop("period must be one positive integer")
  }
  if (anyNA(data)) stop("data must not contain NA values")
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  sma_values <- numeric(length(data) - period + 1)

  for (i in seq_len(length(sma_values))) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}
