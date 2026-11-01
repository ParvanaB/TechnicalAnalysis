# TechnicalAnalysis - Assignment05

Student: Parvana Babali  
Course: BDA400 - Data Science Tools and Techniques  
Assignment: Technical Analysis Using R, Development Phase

## Important implementation rule

All nine technical indicators are implemented with standard base R operations. No existing technical-analysis indicator libraries are used inside the indicator files.

## Files

- `sma.R` - Simple Moving Average
- `ema.R` - Exponential Moving Average
- `macd.R` - Moving Average Convergence Divergence
- `stdev.R` - population standard deviation defined in the assignment
- `linreg.R` - simple linear regression
- `rsi.R` - Relative Strength Index using Wilder smoothing
- `stoch_rsi.R` - Stochastic RSI using the student-written RSI and SMA functions
- `crossover.R` - crossover event detection
- `crossunder.R` - crossunder event detection
- `test_indicators.R` - sample calls and basic assertions
- `apply_to_stock_data.R` - applies the functions to an Assignment 2 stock CSV
- `validation_expected.json` - independently cross-checked expected sample outputs
- `Babali_Parvana_CA_BDA400_A05.pdf` - assignment report

## How to run

1. Open RStudio and set the working directory to the `Assignment05` folder.
2. Run `source("test_indicators.R")` to load and test all functions.
3. To analyze an Assignment 2 stock CSV, copy a file such as `AAPL_data.csv` into this folder and run:

```r
source("apply_to_stock_data.R")
results <- analyze_stock_file("AAPL_data.csv")
print(results)
```

Repository: https://github.com/ParvanaB/TechnicalAnalysis
