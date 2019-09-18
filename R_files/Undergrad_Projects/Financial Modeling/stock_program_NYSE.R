########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
setwd("~/R_Stuff/Stock_Analysis/")

# dependencies
suppressPackageStartupMessages({
  library(foreach)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(quantmod)
  library(doMC)
  options("getSymbols.warning4.0"=FALSE)
  library(knitr)
  library(Quandl)
  source("StockType_Function.R")
  registerDoMC(cores = 4)
})

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
maxretryattempts <- 3#If there is an error downloading a price how many times to retry
if(exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    for(t in 1:maxretryattempts){
      tryCatch(
        {
          #This is the statement to Try
          #Check to see if the variables exists
          #NEAT TRICK ON HOW TO TURN A STRING INTO A VARIABLE
          #SEE  http://www.r-bloggers.com/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
          if(!is.null(eval(parse(text=paste("stockData$",Symbols[m],sep=""))))){
            #The variable exists so dont need to download data for this stock
            #So lets break out of the retry loop and process the next stock
            #cat("No need to retry")
            cat("(",m,"/",Symbols,") ","Downloading ", Symbols[m] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
            break
          }
          # gather price data
          
          getSymb <- try(getSymbols(
            Symbols[m], src = "yahoo", from = year, 
            auto.assign = F)[,6], silent = TRUE) 
        }
        #Specify the catch function, and the finally function
         ,error = function(e) print(e))
    }
  }
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
 
      NA
    }
  }


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.1 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
nrstocks <- length(Symbols)



WoW <- new.env()
##
sapply(Symbols, function(x){
  try(getSymbols(
      x, src = "yahoo", from = year, env=WoW), silent = TRUE)
})

getSymb <- try(getSymbols(
  x, src = "yahoo", from = year, 
  auto.assign = F, env=WoW)[,6], silent = TRUE)




StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      # gather the data iteratively 
      for (i in 1:nrstocks)
      {
        for(t in 1:maxretryattempts){
          tryCatch(
            {
              #This is the statement to Try
              #Check to see if the variables exists
              if(!is.null(eval(parse(text=paste("stockData$",Symbols[i],sep=""))))){
                #The variable exists so dont need to download data for this stock
                #So lets break out of the retry loop and process the next stock
                #cat("No need to retry")
                cat("(",i,"/",nrstocks,") ","Downloading ", Symbols[i] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
                break
              }
              # gather price data
              
              getSymb <- try(getSymbols(
                Symbols[m], src = "yahoo", from = year, 
                auto.assign = F)[,6], silent = TRUE) 
            }
            #Specify the catch function, and the finally function
            ,error = function(e) NULL)
        }
      }
      if(is.xts(getSymb)) {
        
        # which symbols
        updateSymbols <- Symbols[i]
        
        # gather the performance metrics
        what_metrics <- yahooQF(perform_vec)
        metrics <- getQuote(Symbols[i], what=what_metrics)
        
        Time <- time(getSymb) # date vector
        Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
        
        # convert closing prices to returns
        Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
        
        # return as a list
        return(list(Time, Rates, metrics, updateSymbols))
      } else {
        
        NA
      }
    }
    }
  }



######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.2 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.3 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.4 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[801:1000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.5 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1001:1200]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.6 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1201:1400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.7 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1401:1600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.8 <- dataTable
rm(StockDat,dataTable)


########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1601:1800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.9 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1801:2000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.10 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1:1000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo

nrstocks = length(Symbols)
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      stockData <- new.env() 
      nrstocks = length(Symbols)
      maxretryattempts <- 5 #If there is an error downloading a price how many times to retry
      StockDat <-for (i in 1:nrstocks){
        for(t in 1:maxretryattempts){
          tryCatch(
            {
              #This is the statement to Try
              #Check to see if the variables exists
              #NEAT TRICK ON HOW TO TURN A STRING INTO A VARIABLE
              #SEE  http://www.r-bloggers.com/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
              if(!is.null(eval(parse(text=paste("stockData$",Symbols[i],sep=""))))){
                #The variable exists so dont need to download data for this stock
                #So lets break out of the retry loop and process the next stock
                #cat("No need to retry")
                cat("(",i,"/",nrstocks,") ","Downloading ", Symbols[i] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
                # gather price data
                break
              }
              
              # gather price data
              
              # gather price data
              getSymb <- try(getSymbols(
                Symbols[m], src = "yahoo", from = year, 
                auto.assign = F)[,6], silent = TRUE)
            }
            
            
            #Specify the catch function, and the finally function
            , error = function(e) print(e))
          
          
        }
      }
      
      if(is.xts(getSymb)) {
        
        # which symbols
        updateSymbols <- Symbols[i]
        
        # gather the performance metrics
        what_metrics <- yahooQF(perform_vec)
        metrics <- getQuote(Symbols[i], what=what_metrics)
        
        Time <- time(getSymb) # date vector
        Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
        
        # convert closing prices to returns
        Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
        
        # return as a list
        return(list(Time, Rates, metrics, updateSymbols))
      } else {
        NA
      }
    }
  }
    }


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 


dataTable.LSE.11 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[2201:2400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.12 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[2401:2600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.13 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[2601:2800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.LSE.14 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/LSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[2801:3000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out11", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/LSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out11", list.files("Stock.out11/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out11/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^FTMC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out11/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out11", list.files("Stock.out11/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out11/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 


dataTable.LSE.15 <- dataTable

rm(StockDat,dataTable)

LSE.Table <- rbind(dataTable.LSE.1,dataTable.LSE.2,dataTable.LSE.3,dataTable.LSE.4,dataTable.LSE.5,
                   dataTable.LSE.6,dataTable.LSE.7,dataTable.LSE.8,dataTable.LSE.9,dataTable.LSE.10,
                   dataTable.LSE.11,dataTable.LSE.12,dataTable.LSE.13,dataTable.LSE.14,dataTable.LSE.15)
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(LSE.Table, "data_output/stock_stats_LSE_OCT_28_2016.csv")
#__________________________________________________________________

rm(StockDat, dataTable, Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/Euronext.csv", quote="\"", stringsAsFactors=FALSE)$V1[1:200]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                   "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                   "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                   "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                   "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                   "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                   "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out12", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/Euro_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out12", list.files("Stock.out12/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out12/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^N100", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out12", list.files("Stock.out12/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out12/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.EURO.1 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/Euronext.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out12", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/Euro_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out12", list.files("Stock.out12/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out12/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^N100", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out12", list.files("Stock.out12/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out12/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.EURO.2 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/Euronext.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out12", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/Euro_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out12", list.files("Stock.out12/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out12/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^N100", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out12", list.files("Stock.out12/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out12/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.EURO.3 <- dataTable
rm(StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################

# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/Euronext.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:900]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out12", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/Euro_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out12", list.files("Stock.out12/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out12/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^N100", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out12/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out12", list.files("Stock.out12/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out12/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.EURO.4 <- dataTable
rm(StockDat,dataTable)

Euro.Table <- rbind(dataTable.EURO.1,dataTable.EURO.2,dataTable.EURO.3,dataTable.EURO.4)
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(Euro.Table, "data_output/stock_stats_Euronext_OCT_28_2016.csv")

rm(StockDat,dataTable,Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/HKSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1:200]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out13", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/HKSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out13", list.files("Stock.out13/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out13/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.HSK.1 <- dataTable

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/HKSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out13", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/HKSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out13", list.files("Stock.out13/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out13/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.HSK.2 <- dataTable

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/HKSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out13", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/HKSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out13", list.files("Stock.out13/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out13/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.HSK.3 <- dataTable

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/HKSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out13", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/HKSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out13", list.files("Stock.out13/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out13/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.HSK.4 <- dataTable
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/HKSE.csv", quote="\"", stringsAsFactors=FALSE)$V1[1001:2000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out13", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/HKSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out13", list.files("Stock.out13/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out13/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.HSK.5 <- dataTable


dataTable.HSK <- rbind(dataTable.HSK.1,dataTable.HSK.2,dataTable.HSK.3,dataTable.HSK.4,dataTable.HSK.5)

#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable.HSK, "data_output/stock_stats_HKSE_OCT_28_2016.csv")
#__________________________________________________________________

rm(StockDat, dataTable, Volatility_Table)


########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.1 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.2 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.3 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.4 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[801:1000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.5 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[1001:1200]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.6 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[1201:1400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.7 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[1401:1600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.8 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[1601:1800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.9 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[1801:2000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.10 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[2001:2200]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.11 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[2201:2400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.12 <- dataTable

rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1[2401:2600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.13 <- dataTable
rm(StockDat, dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out14", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NASDAQ_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out14", list.files("Stock.out14/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out14/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)

#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^IXIC", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out14/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out14", list.files("Stock.out14/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out14/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NASDAQ.14 <- dataTable
rm(StockDat, dataTable)

Nasdaq.table <- rbind(dataTable.NASDAQ.1,dataTable.NASDAQ.2,dataTable.NASDAQ.3,dataTable.NASDAQ.4,dataTable.NASDAQ.5,
                      dataTable.NASDAQ.6,dataTable.NASDAQ.7,dataTable.NASDAQ.8,dataTable.NASDAQ.9,dataTable.NASDAQ.10,
                      dataTable.NASDAQ.11,dataTable.NASDAQ.12,dataTable.NASDAQ.13,dataTable.NASDAQ.14)
                    




#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable.NASDAQ.14 , "data_output/stock_stats_NASDAQ_DEC_17_2016.csv")

rm(StockDat, dataTable, Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1995-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.1 <- dataTable

rm(StockDat,dataTable,Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.2 <- dataTable
rm(StockDat,dataTable,Volatility_Table)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.3 <- dataTable
rm(StockDat,dataTable,Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:800]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.4 <- dataTable

rm(StockDat,dataTable,Volatility_Table)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[801:1000]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.5 <- dataTable
rm(StockDat,dataTable,Volatility_Table)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="\"", stringsAsFactors=FALSE)$V1[1001:1450]

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/TSX_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out15", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out15", list.files("Stock.out15/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out15/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out15/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out15", list.files("Stock.out15/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out15/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])

dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.TSX.6 <- dataTable
rm(StockDat,dataTable,Volatility_Table)





TSX.TABLE <- rbind(dataTable.TSX.1,dataTable.TSX.2, dataTable.TSX.3,dataTable.TSX.4,dataTable.TSX.5,dataTable.TSX.6)
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "data_output/stock_stats_TSX_OCT_28_2016.csv")
#__________________________________________________________________

rm(Volatility_Table,StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1:200]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.1 <- dataTable

rm(StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[201:400]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.2 <- dataTable
rm(StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[401:600]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.3 <- dataTable
rm(StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[601:800]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.4 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[801:1000]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.5 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1001:1200]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.6 <- dataTable
rm(Volatility_Table,StockDat,dataTable)

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1201:1400]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.7 <- dataTable

rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1401:1600]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.8 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1601:1800]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.9 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[1801:2000]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.10 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[2001:2200]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.11 <- dataTable
rm(Volatility_Table,StockDat,dataTable)
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1[2201:2400]
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.12 <- dataTable
rm(Volatility_Table,StockDat,dataTable)


########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="\"", stringsAsFactors=FALSE)$V1
# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <-c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                 "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                 "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                 "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                 "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                 "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                 "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}

######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/NYSE_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}


#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out16", list.files("Stock.out16/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out16/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out16/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out16", list.files("Stock.out16/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out16/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 

dataTable.NYSE.13 <- dataTable
#rm(Volatility_Table,StockDat,dataTable)



NYSE.TABLE <- rbind(dataTable.NYSE.1,dataTable.NYSE.2,dataTable.NYSE.3,dataTable.NYSE.4,dataTable.NYSE.4,
                    dataTable.NYSE.5,dataTable.NYSE.6,dataTable.NYSE.7,dataTable.NYSE.8,dataTable.NYSE.9,
                    dataTable.NYSE.10,dataTable.NYSE.11,dataTable.NYSE.12,dataTable.NYSE.13)
                    

#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "data_output/stock_stats_NYSE_Entire_DEC_17_2016.csv")
#__________________________________________________________________

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/oilcomptestlist.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out17", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste0("~/R_Stuff/Stock_Analysis/Oil_Tickers.csv/", StockDat[,4][[i]], ".csv"))
  } else {
    print(paste0("missing #",i))
  }
}

#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out17/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out17", list.files("Stock.out17/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out17/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^NYA", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out17/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out17", list.files("Stock.out17/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out17/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 




#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "data_output/stock_stats_NYSE_Oil_OCT_28_2016.csv")
#__________________________________________________________________

########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################
########################################################################
# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


# Read in ticker symbols 
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/oilcomptestlist.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")
# Retrieving stock data from yahoo
# warning provided for stocks not found
StockDat <- if(file.exists(ticker)) 
{
  # if a specified ticker exists, don't re-collect data
  print("You already have data buddy")
  
} else {
  
  # gather the data iteratively 
  foreach(m = 1:length(Symbols), .combine = rbind) %dopar% 
  {
    # gather price data
    getSymb <- try(getSymbols(
      Symbols[m], src = "yahoo", from = year, 
      auto.assign = F)[,6], silent = TRUE)
    
    if(is.xts(getSymb)) {
      
      # which symbols
      updateSymbols <- Symbols[m]
      
      # gather the performance metrics
      what_metrics <- yahooQF(perform_vec)
      metrics <- getQuote(Symbols[m], what=what_metrics)
      
      Time <- time(getSymb) # date vector
      Symb.Numeric <- data.frame(Close = as.numeric(getSymb)) # symbol vector
      
      # convert closing prices to returns
      Rates <- mutate(Symb.Numeric, Close = (Close - lag(Close))/Close)[-1,] 
      
      # return as a list
      return(list(Time, Rates, metrics, updateSymbols))
    } else {
      NA
    }
  }
}


######################################################################################

for(i in 1:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    Stock_df <- data.frame(date = StockDat[,1][[i]][-1], 
                           rate = StockDat[,2][[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out18", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}



#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out18/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out18", list.files("Stock.out18/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T), sum = sum(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out18/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2] , sum = ldply(Volatility_Table[,3])[,2] )
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
capm <- foreach(i = 1:length(list.files("Stock.out18/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out18", list.files("Stock.out18/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out18/"), beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

#*********************** Metrics **********************************
StockDat -> metricDat
metrics <- metricDat[,3][[1]]
for(i in 2:length(Symbols)) {
  if(!is.na(StockDat[,1][[i]][[1]])) {
    metrics <- rbind(metrics, metricDat[,3][[i]])
  }else {"fuck it"}
}

marketString_to_numeric <- function(x)
{
  x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "M", x$Market.Capitalization))], "M"))
  
  x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))] <- 
    as.numeric(strsplit(x$Market.Capitalization[which(grepl(pattern = "B", x$Market.Capitalization))], "B"))*1000
  
  x$EBITDA[which(grepl(pattern = "M", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "M", x$EBITDA))], "M"))
  
  x$EBITDA[which(grepl(pattern = "B", x$EBITDA))] <- 
    as.numeric(strsplit(x$EBITDA[which(grepl(pattern = "B", x$EBITDA))], "B"))*1000
  
  
  x$Market.Capitalization <- as.numeric(x$Market.Capitalization)
  x$EBITDA <- as.numeric(x$EBITDA)
  
  return(x)
}

colnames(metrics)[3] <- "Market.Capitalization"
metric_conversion <- marketString_to_numeric(metrics)[,-1]

vec <- 1:8
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
dataTable <- as.data.frame(inner_join(beta.table, metric_table, by = "ticker"))
dataTable$Earnings.Share <- as.numeric(dataTable$Earnings.Share)
dataTable$X52.week.Low <- as.numeric(dataTable$X52.week.Low)
dataTable$X52.week.High <- as.numeric(dataTable$X52.week.High)
dataTable$Last <- as.numeric(dataTable$Last)
dataTable$P.E.Ratio <- as.numeric(dataTable$P.E.Ratio)
dataTable$Volume <- as.numeric(dataTable$Volume)
dataTable$EPS.Estimate.Current.Year <- as.numeric(dataTable$EPS.Estimate.Current.Year)
dataTable$EPS.Estimate.Next.Year <- as.numeric(dataTable$EPS.Estimate.Next.Year)
dataTable$EPS.Estimate.Next.Quarter  <- as.numeric(dataTable$EPS.Estimate.Next.Quarter)
dataTable$Float.Shares <- as.numeric(dataTable$Float.Shares)
dataTable$Open <- as.numeric(dataTable$Open)
dataTable$Price.Sales  <- as.numeric(dataTable$Price.Sales)
dataTable$Price.Book  <- as.numeric(dataTable$Price.Book)
dataTable$PEG.Ratio <- as.numeric(dataTable$PEG.Ratio)
dataTable$X1.yr.Target.Price   <- as.numeric(dataTable$X1.yr.Target.Price)
dataTable$Dividend.Yield  <- as.numeric(dataTable$Dividend.Yield)
dataTable$Ask <- as.numeric(dataTable$Ask)
dataTable$Bid <- as.numeric(dataTable$Bid)
dataTable$Price.EPS.Estimate.Current.Year <- as.numeric(dataTable$Price.EPS.Estimate.Current.Year)
dataTable$Price.EPS.Estimate.Next.Year <- as.numeric(dataTable$Price.EPS.Estimate.Next.Year)
dataTable$Short.Ratio <- as.numeric(dataTable$Short.Ratio) 




#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "data_output/stock_stats_NYSE_OIL_XOI_OCT_28_2016.csv")
#__________________________________________________________________

