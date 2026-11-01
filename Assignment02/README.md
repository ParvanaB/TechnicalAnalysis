# TechnicalAnalysis - Assignment 02

**Student:** Parvana Babali  
**Student ID:** 664430071  
**Course:** BDA400 - Data Science Tools and Techniques  
**Assignment:** Technical Analysis Using R, Preliminary Stage

## Files

- `portfolio.txt` - stock symbols, one per line
- `analysis.R` - stock import, statistics, display, charting, and CSV export
- `Babali_Parvana_CA_BDA400_A02.pdf` - assignment report
- `screenshots/` - illustrative screenshots extracted from the supplied report

## Portfolio

- AAPL
- MSFT
- GOOGL
- AMZN
- NVDA

## Required packages

- `quantmod`
- `TTR`
- `ggplot2`

The R script installs any missing packages automatically.

## Run

Open RStudio, set the working directory to this `Assignment02` folder, then run:

```r
source("analysis.R")
```

The script downloads approximately one year of daily Yahoo Finance data, computes statistics from adjusted closing prices, prints the first and last rows for each stock, creates adjusted-close/SMA(20) charts, and writes CSV outputs.

## Screenshot note

The screenshots in `screenshots/` were extracted from the supplied report and are explicitly marked **SAMPLE** / illustrative. They are useful for repository documentation but are not presented as proof of an actual GitHub or local RStudio session.


## Screenshot gallery

> These are illustrative screenshots extracted from the supplied report. They remain visibly marked **SAMPLE**.

### GitHub repository layout

![GitHub repository sample](screenshots/C1_GitHub_Repository_SAMPLE.png)

### RStudio setup

![RStudio setup sample](screenshots/C2_RStudio_Setup_SAMPLE.png)

### Package installation

![Package installation sample](screenshots/C3_Package_Installation_SAMPLE.png)

### Stock import and statistics

![Stock analysis sample](screenshots/C4_Stock_Analysis_SAMPLE.png)

### Stock chart

![Stock chart sample](screenshots/C5_Stock_Chart_SAMPLE.png)
