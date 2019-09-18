########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# dependencies
suppressPackageStartupMessages({
  library(foreach)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(quantmod)
  options("getSymbols.warning4.0"=FALSE)
  library(knitr)
  library(Quandl)
})

# Be sure to set your working directory: set * to directory
setwd("~/R_Stuff/")

# Plug year in
#-----------------------------------------
year <- "2015-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- "AXX.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[1:50]

# create empty Symbol vector
Symbols <- rep(0, length(Symbols1))

# for each symbol apply ".TO"
fun <- function(IndexType = "TO", Symbols1 = Symbols1, Symbols = Symbols) {
  if(IndexType == "NYSE") {
    for(i in 1:length(Symbols1)) {
      Symbols[i] <- paste(Symbols1[i], "TO", sep = ".")
    } else {
      for(i in 1:length(Symbols1)) {
        Symbols[i] <- Symbols1[i]
      }
    }
  }
}


# vector of performance metrics
perform_vec <- c("Ask", "Ave. Daily Volume", "Ask Size", "Bid", 
        "Ask (RT)", "Bid (RT)", "Book Value", "Bid Size", "Change & % Change", 
        "Change", "Commission", "Change (RT)", "After Hours Change (RT)", 
        "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
        "Error Indication (returned for symbol changed / invalid)", 
        "EPS Estimate Current Year", "EPS Estimate Next Year", 
        "EPS Estimate Next Quarter", "Float Shares", "Low", "High", 
        "52-week Low", "52-week High", "Holdings Gain %", "Annualized Gain", 
        "Holdings Gain", "Holdings Gain % (RT)", "Holdings Gain (RT)", 
        "More Info", "Order Book (RT)", "Market Capitalization", 
        "Market Cap (RT)", "EBITDA", "Change From 52-week Low", 
        "% Change From 52-week Low", "Last Trade (RT) With Time", 
        "%Change (RT)", "Last Size", "Change From 52-week High", 
        "% Change From 52-week High", "Last", "Last", "High Limit", 
        "Low Limit", "Days Range", "Days Range (RT)", "50-day MA", 
        "200-day MA", "Change From 200-day MA", "% Change From 200-day MA", 
        "Change From 50-day MA", "% Change From 50-day MA", "Name", 
        "Notes", "Open", "P. Close", "Price Paid", "% Change", 
        "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
        "Dividend Pay Date", "P/E Ratio (RT)", "PEG Ratio", "Price/EPS Estimate Current Year", 
        "Price/EPS Estimate Next Year", "Symbol", "Shares Owned", 
        "Short Ratio", "Last Trade Time", "Trade Links", "Ticker Trend", 
        "1 yr Target Price", "Volume", "Holdings Value", "Holdings Value (RT)", 
        "52-week Range", "Days Value Change", "Days Value Change (RT)", 
        "Stock Exchange", "Dividend Yield")

perform_vec <- c( "Symbol", "Symbol", "symbol","Name", "Name", "shortName",
"Name (Long)", "NameLong", "longName",
"Quote Type", "Quote Type", "quoteType",
"Quote Source Name", "Quote Source", "quoteSourceName",
"Source Interval", "Source Interval", "sourceInterval",
"Currency", "Currency", "currency",
"Financial Currency", "Financial Currency", "financialCurrency",
"Market", "Market", "market",
"Market State", "Market State", "marketState",
"Exchange", "Exchange", "exchange",
"Exchange Full Name", "Exchange Full Name", "fullExchangeName",
"Exchange Timezone", "Exchange Timezone", "exchangeTimezoneName",
"Exchange TZ", "Exchange TZ", "exchangeTimezoneShortName",
"Exchange Data Delay", "Exchange Data Delay", "exchangeDataDelayedBy",
"GMT Offset Millis", "GMT Offset", "gmtOffSetMilliseconds",
"Tradeable", "Tradeable", "tradeable",
"Ask", "Ask", "ask",
"Bid", "Bid", "bid",
"Ask Size", "Ask Size", "askSize",
"Bid Size", "Bid Size", "bidSize",
"Last Trade (Price Only)", "Last", "regularMarketPrice",
"Last Trade Time", "Last Trade Time", "regularMarketTime",
"Change", "Change", "regularMarketChange",
"Open", "Open", "regularMarketOpen",
"Days High", "High", "regularMarketDayHigh",
"Days Low", "Low", "regularMarketDayLow",
"Volume", "Volume", "regularMarketVolume",
"Change in Percent", "% Change", "regularMarketChangePercent",
"Previous Close", "P. Close", "regularMarketPreviousClose",
"Trade Date", "Trade Date", "d2",
"Last Trade Size", "Last Size", "k3",
"Last Trade (Real-time) With Time", "Last Trade (RT) With Time", "k1",
"Last Trade (With Time)", "Last", "l",
"High Limit", "High Limit", "l2",
"Low Limit", "Low Limit", "l3",
"Order Book (Real-time)", "Order Book (RT)", "i5",
"Days Range", "Days Range", "m",
"Days Range (Real-time)", "Days Range (RT)", "m2",
"52-week Range", "52-week Range", "w",
"Change From 52-week Low", "Change From 52-week Low", "fiftyTwoWeekLowChange",
"Percent Change From 52-week Low", "% Change From 52-week Low", "fiftyTwoWeekLowChangePercent",
"Change From 52-week High", "Change From 52-week High", "fiftyTwoWeekHighChange",
"Percent Change From 52-week High", "% Change From 52-week High", "fiftyTwoWeekHighChangePercent",
"52-week Low", "52-week Low", "fiftyTwoWeekLow",
"52-week High", "52-week High", "fiftyTwoWeekHigh",
"50-day Moving Average", "50-day MA", "fiftyDayAverage",
"Change From 50-day Moving Average", "Change From 50-day MA", "fiftyDayAverageChange",
"Percent Change From 50-day Moving Average", "% Change From 50-day MA", "fiftyDayAverageChangePercent",
"200-day Moving Average", "200-day MA", "twoHundredDayAverage",
"Change From 200-day Moving Average", "Change From 200-day MA", "twoHundredDayAverageChange",
"Percent Change From 200-day Moving Average", "% Change From 200-day MA", "twoHundredDayAverageChangePercent",
"Market Capitalization", "Market Capitalization", "marketCap",
"Market Cap (Real-time)", "Market Cap (RT)", "j3",
"P/E Ratio", "P/E Ratio", "trailingPE",
"P/E Ratio (Real-time)", "P/E Ratio (RT)", "r2",
"Price/EPS Estimate Current Year", "Price/EPS Estimate Current Year", "r6",
"Price/EPS Estimate Next Year", "Price/EPS Estimate Next Year", "forwardPE",
"Price/Book", "Price/Book", "priceToBook",
"Book Value", "Book Value", "bookValue",
"Price/Sales", "Price/Sales", "p5",
"PEG Ratio", "PEG Ratio", "r5",
"EBITDA", "EBITDA", "j4",
"Average Daily Volume", "Ave. Daily Volume", "averageDailyVolume3Month",
"Average Daily Volume", "Ave. Daily Volume", "averageDailyVolume10Day",
"Shares Outstanding", "Shares Outstanding", "sharesOutstanding",
"Float Shares", "Float Shares", "f6",
"Short Ratio", "Short Ratio", "s7",
"Ex-Dividend Date", "Ex-Dividend Date", "dividendDate",
"Dividend Pay Date", "Dividend Pay Date", "r1",
"Dividend/Share", "Dividend/Share", "trailingAnnualDividendRate",
"Dividend Yield", "Dividend Yield", "trailingAnnualDividendYield",
"Earnings Timestamp", "Earnings Timestamp", "earningsTimestamp",
"Earnings Start Time", "Earnings Start Time", "earningsTimestampStart",
"Earnings End Time", "Earnings End Time", "earningsTimestampEnd",
"Earnings/Share", "Earnings/Share", "epsTrailingTwelveMonths",
"EPS Forward", "EPS Forward", "epsForward",
"Earnings/Share", "Earnings/Share", "e",
"EPS Estimate Current Year", "EPS Estimate Current Year", "e7",
"EPS Estimate Next Year", "EPS Estimate Next Year", "e8",
"EPS Estimate Next Quarter", "EPS Estimate Next Quarter", "e9",
"Language", "Language", "language",
"Message Board ID", "Message Board ID", "messageBoardId",
"Price Hint", "Price Hint", "priceHint",
"Trade Links", "Trade Links", "t6",
"Ticker Trend", "Ticker Trend", "t7",
"1 yr Target Price", "1 yr Target Price", "t8",
"Holdings Value", "Holdings Value", "v1",
"Holdings Value (Real-time)", "Holdings Value (RT)", "v7",
"Days Value Change", "Days Value Change", "w1",
"Days Value Change (Real-time)", "Days Value Change (RT)", "w4",
"Price Paid", "Price Paid", "p1",
"Shares Owned", "Shares Owned", "s1",
"Commission", "Commission", "c3",
"Notes", "Notes", "n4",
"More Info", "More Info", "i",
"Annualized Gain", "Annualized Gain", "g3",
"Holdings Gain", "Holdings Gain", "g4",
"Holdings Gain Percent", "Holdings Gain %", "g1",
"Holdings Gain Percent (Real-time)", "Holdings Gain % (RT)", "g5",
"Holdings Gain (Real-time)", "Holdings Gain (RT)", "g6")


StockDat <- if(file.exists(ticker) == T) {
  
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")} else {
    
    # gather the data iteratively 
    foreach(m = 1:length(Symbols), .combine = rbind) %do% {
      
      # gather price data 
      getSymb <- try(getSymbols(
        Symbols[m], src = "yahoo", from = year, 
        auto.assign = F)[,6], silent = TRUE)
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(stockname, what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics))
    }
    
  }

# helpful loop for disregarding stocks with no data
# stocks with no data retrieved are outputted
for(i in 1:length(Symbols)) {
  Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                         rate = StockDat[,2][[i]])
  rate.logical <- length(StockDat[,2][[i]]) == 0
  
  if(rate.logical == TRUE) {
    print(Symbols[i])
  } else {
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out", Symbols[i], sep = "/"))
  }
}


#############################################################################
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out", list.files("Stock.out/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2], sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)


#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^XOI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out", list.files("Stock.out/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
print(dataTable)


#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "stock_stats_TSXsampleTEST-2016.csv")
#__________________________________________________________________


# plot market sensitivity vs volatility
beta_vs_sd <- ggplot(dataTable, aes(sd, beta)) + 
  geom_point(aes(colour = ticker)) + 
  theme_bw()
ggsave(filename = "beta_vs_sd.png", beta_vs_sd)

# plot PE vs sd
PE_vs_sd <- ggplot(dataTable, aes(y = P.E.Ratio, sd)) +
  geom_point(aes(colour = ticker)) + 
  coord_trans(x = "log10") +
  theme_bw()
ggsave(filename = "PE_vs_sd.png", PE_vs_sd)

