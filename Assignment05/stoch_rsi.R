# BDA400 Assignment 5 - Stochastic RSI (StochRSI)
# Student: Parvana Babali
# Depends only on the student-written rsi() and sma() functions.

stoch_rsi <- function(data, period, k_period, d_period) {
  periods <- c(period, k_period, d_period)
  if (any(!is.numeric(periods)) || any(is.na(periods)) ||
      any(periods <= 0) || any(periods != as.integer(periods))) {
    stop("period, k_period, and d_period must be positive integers")
  }

  rsi_values <- rsi(data, period)
  valid_rsi <- rsi_values[!is.na(rsi_values)]

  if (length(valid_rsi) < k_period) {
    stop("not enough RSI values for k_period")
  }

  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)

  if (max_rsi == min_rsi) {
    normalized_rsi <- rep(0, length(valid_rsi))
  } else {
    normalized_rsi <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  }

  k_line <- sma(normalized_rsi, k_period)

  if (length(k_line) < d_period) {
    stop("not enough %K values for d_period")
  }

  d_line <- sma(k_line, d_period)

  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
