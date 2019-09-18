
#make sure to set your directory
setwd("~/R_Stuff/Stock_Analysis/")


# Read in ticker symbols by using the NASDAQ.csv file or any file in csv to read stock tickers
symbols <- read.table("~/R_Stuff/Stock_Analysis/Nasdaq.NYSE.tickers.csv", quote="", stringsAsFactors=FALSE)$V1


require(jsonlite)
# Creating function for Stock Quotes from Yahoo Finance
getQuote <- function(ticks) {
  qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?formatted=false&symbols="
  z <- fromJSON(paste(qRoot, paste(ticks, collapse=","), sep=""))
  z <- z$quoteResponse$result
  #row.names(z) <- z$symbol
  #z$symbol <- NULL
  #names(z) <- c("ticker", "Time")
  z$regularMarketTime <- as.POSIXct(z$regularMarketTime, origin = '1970-01-01 00:00:00')
  z$earningsTimestamp <- as.POSIXct(z$earningsTimestamp, origin = '1970-01-01 00:00:00')
  z$earningsTimestampStart <- as.POSIXct(z$earningsTimestampStart, origin = '1970-01-01 00:00:00')
  z$earningsTimestampEnd <- as.POSIXct(z$earningsTimestampEnd, origin = '1970-01-01 00:00:00')
  #z$postMarketTime <- as.POSIXct(z$postMarketTime, origin = '1970-01-01 00:00:00')
  z$dividendDate <- as.POSIXct(z$dividendDate, origin = '1970-01-01 00:00:00')
  
  return(z)
}

#tickers <- as.character(beta.df$ticker)
tickers <- symbols

a <- tickers[1:1000]
b <- tickers[1001:2000]
c <- tickers[2001:3000]
d <- tickers[3001:4000]
e <- tickers[4001:5000]
f <- tickers[5001:6000]
g <- tickers[6001:7000]

# CALLING Stock Quotes
metrics1 <- getQuote(a)
metrics2 <- getQuote(b)
metrics3 <- getQuote(c)
metrics4 <- getQuote(d)
metrics5 <- getQuote(e)
metrics6 <- getQuote(f)
metrics7 <- getQuote(g)

# merging dataframes together
library(data.table)
metrics <- rbindlist(list(metrics1,metrics2,metrics3,metrics4,metrics5,metrics6,metrics7),fill=T)

metrics$ticker <- metrics$symbol
#metrics <- cbind(ticker = rownames(metrics), metrics)

metrics <- metrics[!duplicated(metrics$ticker),]
metrics <- data.frame(metrics)

# rounding numbers Function 
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


# rounding to 2 decimal place
metrics <-  round_df(metrics, digits=2)

metrics <- metrics[, c(ncol(metrics), 1:ncol(metrics)-1)]

# put object into CSV file
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(metrics,"Nasdaq-NYSE.csv")
#__________________________________________________________________

