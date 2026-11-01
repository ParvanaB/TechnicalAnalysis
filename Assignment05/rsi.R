# BDA400 Assignment 5 - Relative Strength Index (RSI)
# Student: Parvana Babali
# Uses Wilder's smoothing and no external libraries.

rsi <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (anyNA(data)) stop("data must not contain NA values")
  if (length(period) != 1 || !is.numeric(period) || is.na(period) ||
      period <= 0 || period != as.integer(period)) {
    stop("period must be one positive integer")
  }
  if (length(data) <= period) {
    stop("data length must be greater than period")
  }

  diff_values <- diff(data)
  gains <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, abs(diff_values), 0)

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  rsi_values <- rep(NA_real_, length(data))

  calc_rsi <- function(gain, loss) {
    if (gain == 0 && loss == 0) return(50)
    if (loss == 0) return(100)
    if (gain == 0) return(0)
    rs <- gain / loss
    100 - (100 / (1 + rs))
  }

  rsi_values[period + 1] <- calc_rsi(avg_gain, avg_loss)

  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- calc_rsi(avg_gain, avg_loss)
    }
  }

  return(rsi_values)
}
