# BDA400 Assignment 5 - Integration with Assignment 2 stock data
# Student: Parvana Babali
# This script expects an Assignment 2 CSV such as AAPL_data.csv with an Adjusted or Close column.

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

analyze_stock_file <- function(file) {
  stock <- read.csv(file, stringsAsFactors = FALSE)
  price_col <- if ("Adjusted" %in% names(stock)) "Adjusted" else if ("Close" %in% names(stock)) "Close" else NA_character_
  if (is.na(price_col)) stop("CSV must contain Adjusted or Close prices")
  prices <- stock[[price_col]]
  prices <- prices[is.finite(prices)]
  if (length(prices) < 30) stop("At least 30 valid prices are required")

  ema_fast <- ema(prices, 12)
  ema_slow <- ema(prices, 26)

  list(
    sma20 = sma(prices, 20),
    ema12 = ema_fast,
    ema26 = ema_slow,
    macd = macd(prices, 12, 26, 9),
    stdev = stdev(prices),
    linreg20 = linreg(prices, 20, 0),
    rsi14 = rsi(prices, 14),
    stoch_rsi = stoch_rsi(prices, 14, 3, 3),
    ema_crossovers = crossover(ema_fast, ema_slow),
    ema_crossunders = crossunder(ema_fast, ema_slow)
  )
}

# Example after placing an Assignment 2 output CSV in this folder:
# results <- analyze_stock_file("AAPL_data.csv")
# print(results)
