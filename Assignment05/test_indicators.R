# BDA400 Assignment 5 - Validation Test Script
# Student: Parvana Babali

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("=== SMA ===\n")
data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(sma(data1, 3))

cat("\n=== EMA ===\n")
print(ema(data1, 3))

cat("\n=== MACD ===\n")
data2 <- c(100, 105, 110, 115, 120, 125, 130)
print(macd(data2, 3, 5, 2))

cat("\n=== STANDARD DEVIATION ===\n")
print(stdev(data1))

cat("\n=== LINEAR REGRESSION ===\n")
print(linreg(data1, regressionLength = 5, regressionOffset = 0))

cat("\n=== RSI ===\n")
data3 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
print(rsi(data3, 5))

cat("\n=== STOCHASTIC RSI ===\n")
data4 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
           67, 70, 68, 72, 75, 73, 77, 80, 78, 82)
print(stoch_rsi(data4, period = 5, k_period = 3, d_period = 3))

cat("\n=== CROSSOVER ===\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
print(crossover(arr1, arr2))

cat("\n=== CROSSUNDER ===\n")
print(crossunder(arr1, arr2))

# Simple assertions for known values.
stopifnot(all.equal(sma(data1, 3)[1], 37 / 3))
stopifnot(abs(stdev(c(1, 2, 3)) - sqrt(2 / 3)) < 1e-12)
stopifnot(crossover(c(1, 3), c(2, 2))[2] == "Up")
stopifnot(crossunder(c(3, 1), c(2, 2))[2] == "True")

cat("\nAll basic validation checks completed.\n")
