# TechnicalAnalysis - Assignment 06

**Student:** Parvana Babali  
**Course:** BDA400 - Data Science Tools and Techniques  
**Assignment:** Technical Analysis Using R, Visualization Stage  
**Repository:** https://github.com/ParvanaB/TechnicalAnalysis/tree/main/Assignment06

## Project Summary

This assignment implements an interactive R Shiny portfolio dashboard that retrieves stock-market data from Yahoo Finance and visualizes technical-analysis information.

The dashboard includes:

- Yahoo Finance stock-data retrieval with error handling
- Portfolio symbol selection plus support for manually entered symbols
- Date-range filtering
- Daily, weekly, and monthly time frames
- Line, area, and candlestick charts
- Toggleable Moving Average, RSI, and MACD indicators
- Customizable short/long moving-average periods
- Customizable RSI period and optional RSI signal confirmation
- Buy, Sell, and Hold trading logic
- Buy/Sell annotations on the price chart
- Summary statistics, recent signals, and latest-data tables

## Files

- `app.R` - complete Shiny dashboard
- `setup_packages.R` - installs required packages
- `portfolio.txt` - portfolio symbols
- `Babali_Parvana_CA_BDA400_A06.pdf` - LMS cover/report
- `Babali_Parvana_CA_BDA400_A06.docx` - editable report
- `validation_checklist.txt` - review checklist

## Required R Packages

- `shiny`
- `ggplot2`
- `quantmod`
- `TTR`

Run:

```r
source("setup_packages.R")
```

## Run the Dashboard

From RStudio, set the working directory to this `Assignment06` folder and run:

```r
shiny::runApp()
```

Alternatively, open `app.R` and click **Run App**.

## Trading Rules

### MA Crossover

- **Buy:** short moving average crosses above the long moving average.
- **Sell:** short moving average crosses below the long moving average.
- **Hold:** no new crossover occurs.

### MA + RSI Confirmation

The user may optionally require:

- Buy crossover + RSI at or below the selected buy threshold.
- Sell crossover + RSI at or above the selected sell threshold.

## Indicator Display

Moving averages are overlaid directly on the stock-price chart. RSI and MACD can each be turned on or off and are shown in their own synchronized panels because they use different numeric scales from stock price.

## Data Source

Yahoo Finance is accessed through `quantmod::getSymbols()`.
