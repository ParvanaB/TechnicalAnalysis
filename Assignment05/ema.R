# BDA400 Assignment 5 - Exponential Moving Average (EMA)
# Student: Parvana Babali
# Base R only - no indicator libraries are used.

ema <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period <= 0 || period != as.integer(period)) {
    stop("period must be one positive integer")
  }
  if (length(data) == 0) stop("data must contain at least one value")
  if (anyNA(data)) stop("data must not contain NA values")

  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))

  ema_values[1] <- data[1]

  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier +
        ema_values[i - 1]
    }
  }

  return(ema_values)
}
