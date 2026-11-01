# BDA400 Assignment 02 | Parvana Babali | 664430071
# Run from the Assignment02 folder. Requires internet access for Yahoo Finance.

required <- c('quantmod', 'TTR', 'ggplot2')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) install.packages(missing, repos='https://cloud.r-project.org')

library(quantmod)
library(TTR)
library(ggplot2)

load_stock_data <- function(file='portfolio.txt', from=Sys.Date()-365, to=Sys.Date()) {
    if (!file.exists(file)) stop('Missing portfolio file: ', file)
    symbols <- unique(trimws(readLines(file, warn=FALSE)))
    symbols <- symbols[nzchar(symbols) & !grepl('^#', symbols)]
    if (!length(symbols)) stop('Portfolio contains no symbols')
    result <- list()

    for (symbol in symbols) {
        tryCatch({
            x <- quantmod::getSymbols(
                symbol,
                src='yahoo',
                from=from,
                to=to,
                auto.assign=FALSE,
                warnings=FALSE
            )

            result[[symbol]] <- data.frame(
                Date=as.Date(index(x)),
                Open=as.numeric(Op(x)),
                High=as.numeric(Hi(x)),
                Low=as.numeric(Lo(x)),
                Close=as.numeric(Cl(x)),
                Volume=as.numeric(Vo(x)),
                Adjusted=as.numeric(Ad(x))
            )

            message('Loaded ', symbol, ': ', nrow(result[[symbol]]), ' observations')
        }, error=function(e) {
            warning('Unable to load ', symbol, ': ', conditionMessage(e))
        })
    }

    if (!length(result)) stop('No stocks loaded. Check connection and symbols.')
    result
}

# Statistical mode for continuous prices: the most frequent observed rounded price;
# ties are resolved by choosing the smallest price. Not base R mode() (data type).
price_mode <- function(x, digits=2) {
    x <- round(x[is.finite(x)], digits)
    if (!length(x)) return(NA_real_)
    tab <- table(x)
    min(as.numeric(names(tab)[tab == max(tab)]))
}

calculate_statistics <- function(stock_df, symbol='Unknown', window=20) {
    stopifnot(is.data.frame(stock_df), 'Adjusted' %in% names(stock_df), window >= 1)
    prices <- stock_df$Adjusted
    valid <- prices[is.finite(prices)]

    if (!length(valid)) stop('No valid adjusted closing prices')

    ma <- TTR::SMA(prices, n=window)

    summary <- data.frame(
        Symbol=symbol,
        Observations=length(valid),
        Mean=mean(valid),
        Mode=price_mode(valid),
        Median=median(valid),
        SD=if(length(valid)>1) sd(valid) else NA_real_,
        LatestAdjusted=tail(valid, 1),
        LatestSMA=if(all(is.na(ma))) NA_real_ else tail(na.omit(ma), 1),
        stringsAsFactors=FALSE
    )

    list(summary=summary, moving_average=ma)
}

display_stock <- function(stock_df, symbol, stats, output_dir='outputs') {
    dir.create(output_dir, showWarnings=FALSE, recursive=TRUE)

    cat('\n--- ', symbol, ' ---\n', sep='')
    print(utils::head(stock_df, 6))
    print(utils::tail(stock_df, 6))
    print(stats$summary, row.names=FALSE)

    plot_df <- data.frame(
        Date=stock_df$Date,
        Adjusted=stock_df$Adjusted,
        SMA20=as.numeric(stats$moving_average)
    )

    p <- ggplot(plot_df, aes(Date)) +
        geom_line(aes(y=Adjusted, colour='Adjusted close'), linewidth=0.6) +
        geom_line(aes(y=SMA20, colour='20-day SMA'), linewidth=0.7, na.rm=TRUE) +
        labs(
            title=paste(symbol, 'Adjusted Price and Moving Average'),
            x='Trading date',
            y='Price (USD)',
            colour='Series'
        ) +
        theme_minimal()

    print(p)

    ggsave(
        file.path(output_dir, paste0(symbol, '_chart.png')),
        p,
        width=9,
        height=5,
        dpi=150
    )

    utils::write.csv(
        stock_df,
        file.path(output_dir, paste0(symbol, '_data.csv')),
        row.names=FALSE
    )
}

run_analysis <- function() {
    stocks <- load_stock_data()

    stats <- lapply(
        names(stocks),
        function(s) calculate_statistics(stocks[[s]], s)
    )
    names(stats) <- names(stocks)

    for (s in names(stocks)) {
        display_stock(stocks[[s]], s, stats[[s]])
    }

    summary_table <- do.call(
        rbind,
        lapply(stats, `[[`, 'summary')
    )

    print(summary_table, row.names=FALSE)

    dir.create('outputs', showWarnings=FALSE, recursive=TRUE)
    utils::write.csv(
        summary_table,
        'outputs/portfolio_statistics.csv',
        row.names=FALSE
    )

    invisible(list(
        data=stocks,
        statistics=stats,
        summary=summary_table
    ))
}

if (sys.nframe() == 0) results <- run_analysis()
