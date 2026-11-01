# BDA400 Assignment 5 - Moving Average Convergence Divergence (MACD)
# Student: Parvana Babali
# Depends only on the student-written ema() function in ema.R.

macd <- function(data, short_period, long_period, signal_period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (anyNA(data)) stop("data must not contain NA values")

  periods <- c(short_period, long_period, signal_period)
  if (any(!is.numeric(periods)) || any(is.na(periods)) ||
      any(periods <= 0) || any(periods != as.integer(periods))) {
    stop("all periods must be positive integers")
  }
  if (short_period >= long_period) {
    stop("short_period must be less than long_period")
  }

  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line

  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}
