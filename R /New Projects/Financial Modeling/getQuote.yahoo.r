# getQuote should function like getSymbols
# getQuote.yahoo
# getQuote.IBrokers
# getQuote.RBloomberg
# getQuote.OpenTick

`getQuote` <-
  function(Symbols,src='yahoo',what, ...) {
    args <- list(Symbols=Symbols,...)
    if(!missing(what))
      args$what <- what
    do.call(paste('getQuote',src,sep='.'), args)
  }

`getQuote.yahoo` <-
  function(Symbols,what=standardQuote(),...) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    if(length(Symbols) > 1 && is.character(Symbols))
      Symbols <- paste(Symbols,collapse=";")
    length.of.symbols <- length(unlist(strsplit(Symbols, ";")))
    if(length.of.symbols > 200) {
      # yahoo only works with 200 symbols or less per call
      # we will recursively call getQuote.yahoo to handle each block of 200
      Symbols <- unlist(strsplit(Symbols,";"))
      all.symbols <- lapply(seq(1,length.of.symbols,200),
                            function(x) na.omit(Symbols[x:(x+199)]))
      df <- NULL
      cat("downloading set: ")
      for(i in 1:length(all.symbols)) {
        Sys.sleep(0.5)
        cat(i,", ")
        df <- rbind(df, getQuote.yahoo(all.symbols[[i]],what))
      }
      cat("...done\n")
      return(df)
    }
    Symbols <- paste(strsplit(Symbols,';')[[1]],collapse="+")
    if(inherits(what, 'quoteFormat')) {
      QF <- what[[1]]
      QF.names <- what[[2]]
    } else {
      QF <- what
      QF.names <- NULL
    }
    QF <- paste('d1t1',QF,sep='')
    download.file(paste(
      "https://finance.yahoo.com/d/quotes.csv?s=",
      Symbols,
      "&f=",QF,sep=""),
      destfile=tmp,quiet=TRUE)
    sq <- read.csv(file=tmp,sep=',',stringsAsFactors=FALSE,header=FALSE)
    Qposix <- strptime(paste(sq[,1],sq[,2]),format='%m/%d/%Y %H:%M')
    Symbols <- unlist(strsplit(Symbols,'\\+'))
    df <- data.frame(Qposix,sq[,3:NCOL(sq)])
    rownames(df) <- Symbols
    if(!is.null(QF.names)) {
      colnames(df) <- c('Trade Time',QF.names)
    }
    df
  }


# integrate this into the main getQuote.yahoo, after branching that
#
`getAllQuotes` <-
  function() {
    st <- seq(1,3000,200)
    en <- seq(200,3000,200)
    aq <- NULL
    for(i in 1:length(st)) {
      cc <- getQuote(paste(read.csv(options()$symbolNamesFile.NASDAQ, sep='|')$Sym[seq(st[i],en[i])],collapse=';'))
      cat('finished first',en[i],'\n')
      Sys.sleep(.1)
      aq <- rbind(aq,cc)
    }
    aq
  }


`standardQuote` <- function(src='yahoo') {
  do.call(paste('standardQuote',src,sep='.'),list())
}

`standardQuote.yahoo` <- function() {
  yahooQF(names=c( "Last Trade (Price Only)",
                   "Change","Change in Percent",
                   "Open", "Days High", "Days Low", "Volume"))
}

yahooQuote.EOD <- structure(list("ohgl1v", c("Open", "High",
                                             "Low", "Close",
                                             "Volume")), class="quoteFormat")

`yahooQF` <- function(names) {
  optnames <- c("Ask", "Average Daily Volume", "Ask Size", "Bid", "Ask (Real-time)", 
                "Bid (Real-time)", "Book Value", "Bid Size", "Change & Percent Change", 
                "Change", "Commission", "Change (Real-time)", "After Hours Change (Real-time)", 
                "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
                "Error Indication (returned for symbol changed / invalid)", "EPS Estimate Current Year", 
                "EPS Estimate Next Year", "EPS Estimate Next Quarter", "Float Shares", 
                "Days Low","Days High", "52-week Low", "52-week High", "Holdings Gain Percent", 
                "Annualized Gain", "Holdings Gain", "Holdings Gain Percent (Real-time)", 
                "Holdings Gain (Real-time)", "More Info", "Order Book (Real-time)", 
                "Market Capitalization", "Market Cap (Real-time)", "EBITDA", 
                "Change From 52-week Low", "Percent Change From 52-week Low", 
                "Last Trade (Real-time) With Time", "Change Percent (Real-time)", 
                "Last Trade Size", "Change From 52-week High", "Percent Change From 52-week High", 
                "Last Trade (With Time)", "Last Trade (Price Only)", "High Limit", 
                "Low Limit", "Days Range","Days Range (Real-time)", "50-day Moving Average", 
                "200-day Moving Average", "Change From 200-day Moving Average", 
                "Percent Change From 200-day Moving Average", "Change From 50-day Moving Average", 
                "Percent Change From 50-day Moving Average", "Name", "Notes", 
                "Open", "Previous Close", "Price Paid", "Change in Percent", 
                "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
                "Dividend Pay Date", "P/E Ratio (Real-time)", 
                "PEG Ratio", "Price/EPS Estimate Current Year", 
                "Price/EPS Estimate Next Year", "Symbol", "Shares Owned", "Short Ratio", 
                "Last Trade Time", "Trade Links", "Ticker Trend", "1 yr Target Price", 
                "Volume", "Holdings Value", "Holdings Value (Real-time)", "52-week Range", 
                "Days Value Change", "Days Value Change (Real-time)", "Stock Exchange", 
                "Dividend Yield")
  optshort <- c("Ask", "Ave. Daily Volume", "Ask Size", "Bid", "Ask (RT)", 
                "Bid (RT)", "Book Value", "Bid Size", "Change & % Change", 
                "Change", "Commission", "Change (RT)", "After Hours Change (RT)", 
                "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
                "Error Indication (returned for symbol changed / invalid)",
                "EPS Estimate Current Year", 
                "EPS Estimate Next Year", "EPS Estimate Next Quarter", "Float Shares", 
                "Low","High", "52-week Low", "52-week High", "Holdings Gain %", 
                "Annualized Gain", "Holdings Gain", "Holdings Gain % (RT)", 
                "Holdings Gain (RT)", "More Info", "Order Book (RT)", 
                "Market Capitalization", "Market Cap (RT)", "EBITDA", 
                "Change From 52-week Low", "% Change From 52-week Low", 
                "Last Trade (RT) With Time", "%Change (RT)", 
                "Last Size", "Change From 52-week High", "% Change From 52-week High", 
                "Last", "Last", "High Limit", 
                "Low Limit", "Days Range","Days Range (RT)", "50-day MA", 
                "200-day MA", "Change From 200-day MA", 
                "% Change From 200-day MA", "Change From 50-day MA", 
                "% Change From 50-day MA", "Name", "Notes", 
                "Open", "P. Close", "Price Paid", "% Change", 
                "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
                "Dividend Pay Date", "P/E Ratio (RT)", 
                "PEG Ratio", "Price/EPS Estimate Current Year", 
                "Price/EPS Estimate Next Year", "Symbol", "Shares Owned", "Short Ratio", 
                "Last Trade Time", "Trade Links", "Ticker Trend", "1 yr Target Price", 
                "Volume", "Holdings Value", "Holdings Value (RT)", "52-week Range", 
                "Days Value Change", "Days Value Change (RT)", "Stock Exchange", 
                "Dividend Yield")
  optcodes <- c("a", "a2", "a5", "b", "b2", "b3", "b4", "b6", "c", "c1", "c3", 
                "c6", "c8", "d", "d1", "d2", "e", "e1", "e7", "e8", "e9", "f6", 
                "g", "h", "j", "k", "g1", "g3", "g4", "g5", "g6", "i", "i5", "j1", 
                "j3", "j4", "j5", "j6", "k1", "k2", "k3", "k4", "k5", "l", "l1", 
                "l2", "l3", "m", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "n", "n4", 
                "o", "p", "p1", "p2", "p5", "p6", "q", "r", "r1", "r2", "r5", 
                "r6", "r7", "s", "s1", "s7", "t1", "t6", "t7", "t8", "v", "v1", 
                "v7", "w", "w1", "w4", "x", "y")
  w <- NULL
  
  if(!missing(names)) {
    names <- unlist(strsplit(names,';'))
    for(n in names) {
      w <- c(w,which(optnames %in% n))
    }
  } else {
    names <- select.list(optnames, multiple=TRUE)
    for(n in names) {
      w <- c(w,which(optnames %in% n))
    }
  }
  str <- paste(optcodes[w],collapse='')
  nms <- optshort[w]
  return(structure(list(str,nms),class='quoteFormat'))
}



###############################################################################################
GetQuoteData <-
  function(tickers) {
    library(jsonlite)
    library(stringr)
    x = unique(tickers)
    fields <-
      c(
        "regularMarketTime"
        ,
        "shortName"
        ,
        "symbol"
        ,
        "regularMarketPrice"
        ,
        "regularMarketChangePercent"
        ,
        "regularMarketDayLow"
        ,
        "regularMarketDayHigh"
        ,
        "fiftyTwoWeekLow"
        ,
        "fiftyTwoWeekHigh"
        ,
        "marketCap"
        ,
        "fiftyTwoWeekLowChangePercent"
        ,
        "fiftyTwoWeekHighChangePercent"
        ,
        "bidSize"
        ,
        "fiftyDayAverage"
        ,
        "twoHundredDayAverage"
        ,
        "regularMarketOpen"
        ,
        "trailingPE"
        ,
        "regularMarketPreviousClose"
      )
    
    x <-
      fromJSON(
        paste(
          "https://query1.finance.yahoo.com/v7/finance/quote?",
          "formatted=false&symbols=",
          paste(x, collapse = ","),
          sep = ""
        )
      )
    x = as.data.frame(x$quoteResponse$result, stringsAsFactors = F)
    x$EBITDA <- NA
    #x$`Last Trade Size` <- NA
    x$`PEG Ratio` <- NA
    missing <- setdiff(fields, names(x))
    x[missing] <- NA
    newNames <-
      c(
        "regularMarketTime"
        ,
        "shortName"
        ,
        "symbol"
        ,
        "regularMarketPrice"
        ,
        "regularMarketChangePercent"
        ,
        "regularMarketDayLow"
        ,
        "regularMarketDayHigh"
        ,
        "fiftyTwoWeekLow"
        ,
        "fiftyTwoWeekHigh"
        ,
        "marketCap"
        ,
        "EBITDA"
        ,
        "fiftyTwoWeekLowChangePercent"
        ,
        "fiftyTwoWeekHighChangePercent"
        ,
        "bidSize"
        ,
        "fiftyDayAverage"
        ,
        "twoHundredDayAverage"
        ,
        "regularMarketOpen"
        ,
        "trailingPE"
        ,
        "PEG Ratio"
        ,
        "regularMarketPreviousClose"
      )
    
    oldNames <- c(
      "Trade Time"
      ,
      "Name"
      ,
      "Symbol"
      ,
      "Last"
      ,
      "% Change"
      ,
      "Low"
      ,
      "High"
      ,
      "52-week Low"
      ,
      "52-week High"
      ,
      "Market Capitalization"
      ,
      "EBITDA"
      ,
      "% Change From 52-week Low"
      ,
      "% Change From 52-week High"
      ,
      "Last Size"
      ,
      "50-day MA"
      ,
      "200-day MA"
      ,
      "Open"
      ,
      "P/E Ratio"
      ,
      "PEG Ratio"
      ,
      "P. Close"
    )
    
    x <- x[, c(newNames)]
    
    names(x) <- oldNames
    
    rownames(x) = NULL
    
    x$`Trade Time` = as.character(as.POSIXct(x$`Trade Time`, origin = '1970-01-01 00:00:00', tz = "EST"))
    
    x$changeNum1 = round(as.numeric(gsub("%", "", x$`% Change`)), 3)
    
    x$`% Change` = paste0(ifelse(!grepl("-", x$`% Change`), "+", ""),
                          format(round(as.numeric(
                            gsub("%", "", x$`% Change`)
                          ), 3), trim = T, nsmall = 3),
                          "%")
    
    x$changeNum52l = round(as.numeric(gsub("%", "", x$`% Change From 52-week Low`)), 3)
    
    x$`% Change From 52-week Low` = paste0(ifelse(!grepl("-", x$`% Change From 52-week Low`), "+", ""),
                                           format(round(as.numeric(
                                             gsub("%", "", x$`% Change From 52-week Low`)
                                           ), 3), trim = T, nsmall = 3),
                                           "%")
    
    x$changeNum52h = round(as.numeric(gsub("%", "", x$`% Change From 52-week High`)), 3)
    
    x$`% Change From 52-week High` = paste0(ifelse(!grepl("-", x$`% Change From 52-week High`), "+", ""),
                                            format(round(as.numeric(
                                              gsub("%", "", x$`% Change From 52-week High`)
                                            ), 3), trim = T, nsmall = 3),
                                            "%")
    
    x$Last = prettyNum(format(round(as.numeric(x$Last), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$Low = prettyNum(format(round(as.numeric(x$Low), 2), nsmall = 2),
                      big.mark = ",",
                      preserve.width = "none")
    
    x$High = prettyNum(format(round(as.numeric(x$High), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$`52-week Low` = prettyNum(format(round(as.numeric(x$`52-week Low`), 2), nsmall = 2),
                                big.mark = ",",
                                preserve.width = "none")
    
    x$`52-week High` = prettyNum(format(round(as.numeric(x$`52-week High`), 2), nsmall = 2),
                                 big.mark = ",",
                                 preserve.width = "none")
    
    x$`Last Size` = prettyNum(round(as.numeric(x$`Last Size`) * 100, 2),
                              big.mark = ",",
                              preserve.width = "none")
    
    x$`50-day MA` = prettyNum(format(round(as.numeric(
      gsub(",", "", x$`50-day MA`)
    ), 2), nsmall = 2),
    big.mark = ",",
    preserve.width = "none")
    
    x$`200-day MA` = prettyNum(format(round(as.numeric(
      gsub(",", "", x$`200-day MA`)
    ), 2), nsmall = 2),
    big.mark = ",",
    preserve.width = "none")
    
    x$Open = prettyNum(format(round(as.numeric(x$Open), 2), nsmall = 2),
                       big.mark = ",",
                       preserve.width = "none")
    
    x$`P. Close` = prettyNum(format(round(as.numeric(x$`P. Close`), 2), nsmall = 2),
                             big.mark = ",",
                             preserve.width = "none")
    
    x$`P/E Ratio` = round(as.numeric(x$`P/E Ratio`), 2)
    
    x$`PEG Ratio` = round(as.numeric(x$`PEG Ratio`), 2)
    
    x$Marqhtml = paste0('<span class ="marq">'
                        ,
                        x$Symbol
                        ,
                        " "
                        ,
                        ifelse(
                          grepl("\\+", x$`% Change`)
                          ,
                          paste0(
                            '<span style="color:green;">'
                            ,
                            " "
                            ,
                            x$Last
                            ,
                            ' &#9650</span></span>'
                          )
                          ,
                          paste0(
                            '<span style="color:red;">'
                            ,
                            " "
                            ,
                            x$Last
                            ,
                            ' &#9660</span></span>'
                          )
                        ))
    
    x$UpDn = paste0(ifelse(
      grepl("\\+", x$`% Change`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    , " "
    , x$`% Change`)
    
    
    x$`% Chg frm 52wk Lo` = paste0(ifelse(
      grepl("\\+", x$`% Change From 52-week Low`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    ,
    " "
    ,
    x$`% Change From 52-week Low`)
    
    
    x$`% Chg frm 52wk Hi` = paste0(ifelse(
      grepl("\\+", x$`% Change From 52-week High`)
      ,
      paste0('\u25B2')
      ,
      paste0('\U25BC')
    )
    ,
    " "
    ,
    x$`% Change From 52-week High`)
    
    
    
    
    
    x$xUpDn = paste0(x$`% Change`
                     , " "
                     , ifelse(
                       grepl("\\+", x$`% Change`)
                       ,
                       paste0('\u25B2')
                       ,
                       paste0('\U25BC')
                     ))
    
    x = x[, c(1:20, 24:28, 21:23)]
    
    colnames(x)[8:10] = c("52wk Lo", "52wk Hi", "Market Cap")
    x$LastSizeNum <-
      as.numeric(gsub(",", "", x$`Last Size`)) # x$changeNum1))
    x$lastPriceNum <- round(as.numeric(gsub(",", "", x$Last)), 2)
    x$SMA50Num <- round(as.numeric(gsub(",", "", x$`50-day MA`)), 2)
    x$SMA200Num <-
      round(as.numeric(gsub(",", "", x$`200-day MA`)), 2)
    x$MktCapNum <- suppressWarnings(as.numeric(x$`Market Cap`))
    x$`Market Cap` <-
      paste(round(x$`Market Cap` / 10 ^ 9, 3), "B", sep = "")
    
    return(x)
  }

getQuote <- function(ticks) {
  qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,longName,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols="
  z <- fromJSON(paste(qRoot, paste(ticks, collapse=","), sep=""))
  z <- z$quoteResponse$result[,c("symbol", "regularMarketTime", "regularMarketPrice", "regularMarketChange", "longName")]
  row.names(z) <- z$symbol
  z$symbol <- NULL
  names(z) <- c("Time", "Price", "Change", "Name")
  z$Time <- as.POSIXct(z$Time, origin = '1970-01-01 00:00:00')
  return(z)
}

a <- getQuote("aapl")
test <- symbols[1:5]

c <- GetQuoteData(test)

