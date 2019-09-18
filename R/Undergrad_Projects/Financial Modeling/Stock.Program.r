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


# Be sure to set your working directory: set * to directorysetwd("~/R_Stuff/Stock_Analysis/")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------
#Ticker_Files/stock_stats_US_Undervalued_tickers_10.05.2016.csv
#
# Read in ticker symbols by using the NASDAQ.csv file
symbols <- read.table("Ticker_Files/oilcomptestlist.csv", quote="", stringsAsFactors=FALSE)$V1
#stock_stats_US_Undervalued_tickers_.09.27.2016.csv
#stock_stats_US_Undervalued_tickers_10.05.2016.csv

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

#data.env <- new.env()
dataset <- xts()
# cool progress bar to see the % of completion
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)


# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from=year, src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from=year, src='yahoo')
    dataset <- merge(dataset, Ad(get(symbols[i])))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}


str(dataset)

#dat  <- data.frame(date=index(dataset), coredata(dataset))
#index(dataset) <- round(index(dataset),"day")


rm(list=setdiff(ls(), c("dataset", "perform_vec","symbols","year", "getQuote")))
#rm(list=setdiff(ls(), c("dataset", "perform_vec","symbols","year", "getQuote")))

dat$date <- as.factor(dat$date)

dat1 <- as.data.frame(dataset)
nr <- nrow(dat1)
dat1 <- (dat1[-1,] - dat1[-nr,]) / dat1[-nr,]



library(functional)

do.call(data.frame,lapply(dat1, function(x) replace(x, is.infinite(x),NA)))




dat  <- data.frame(date=round(index(dataset,"days")), coredata(dataset))
dat <- cbind(ticker = rownames(dat), dat)
rownames(dat) <- 1:nrow(dat)
date  <- dat[,2]
dat <- dat[,-c(1,2)]

date <- round(date,"days")

#dat <- cbind(date,dat)

dat <- dat[,colSums(is.na(dat))<nrow(dat)]
#df <- dat[, !apply(is.na(dat) | dat ==0, 2, all)]
#df <- df[colSums(!is.na(df)) > 10 ]


#dat <- dat[,colVars(is.na(dat))<nrow(dat)]

#df <- Filter(function(x) length(unique(x[!is.na(x)])) > 1, dat)
#df[, colSums(is.na(df)) != nrow(df)]

#results <- colSums(dat,na.rm=TRUE)

#all_Desc <- dat[, !apply(dat, 2, function(x) any(is.na(x)))]

#colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)

str(dat)

stockname <- colnames(dat)

#stockname <-gsub("X", "", stockname)
#data <- dat[100:200]

stockname <-gsub(".Adjusted", "", stockname)




data <- gsub('[.]', '-', stockname)
stockname <- gsub('\\-TO*', '.TO', data)




for(i in 1:length(stockname)) {
  if(exists("dat")) {
    Stock_df <- data.frame( date = date, rate = dat[[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out17", stockname[i],sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


# 'http://finance.google.com/finance/info?client=ig&q=',n,sep=''
# https://finance.google.com/finance/getprices?q=

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

getQuote1 <- function(ticks) {
  url <- paste('https://finance.google.com/finance?q=',n,'&output=json',sep='')
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



#what_metrics <- yahooQF(perform_vec)
#metrics <- getQuote(stockname, what=what_metrics)

NYSE.returns <- as.data.frame(oil.NYSE)
NYSE.returns <- cbind(date = rownames(NYSE.returns), NYSE.returns)
#rownames(metrics) <- 1:nrow(metrics)



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

#check nan inf
#which(is.na(Volatility_Table), arr.ind=TRUE)
a<-  as.data.frame(Volatility_Table[is.na(Volatility_Table$sd),]) 
a
#remove files in folder
setwd("~/R_Stuff/Stock_Analysis/Stock.out17")
file.removed <- as.character(a[,1])
str(file.removed)
Sys.sleep(3)

junk <- dir(path="Stock.out17/", pattern=file.removed) # ?dir
file.remove(file.removed) # ?file.remove#
setwd("~/R_Stuff/Stock_Analysis")
Sys.sleep(4)

Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
Volatility_Table <- na.omit(Volatility_Table)


#*********************** Betas **********************************
# get data
# Plug year in
#-----------------------------------------
fromDate <- "2012-11-9" # <== plug in here
#-----------------------------------------

oil.NYSE <- getSymbols("^GSPC", src = "yahoo", from = year, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
# 10y BOC/ V39055  5y -V39053
CAD1 <- Quandl("BOC/V39052", start_date=year, collapse="daily")

n <- nrow(CAD1)

CAD <- xts(CAD1[,-1], order.by=as.Date(CAD1[,1], "%m/%d/%Y"))

rf <- CAD1[1,2]/100
#CAD.returns <- data.frame(date = as.factor(time(CAD)), price = as.numeric(CAD))  %>%
##  mutate(rate.CAD = (price - lag(price))/lag(price)) %>%
#  filter(date != year)  %>% 
#  select(-price)
#dailyYield <- (1+(Cl(IRX)/100))^(1/252) - 1
CAD.returns <- data.frame(date = as.factor(time(CAD)), price = as.numeric(CAD))  %>%
  mutate(rate.CAD = ((1+(price))^(1/365)) - 1) %>%
  filter(date != year)  %>% 
  select(-price)



# get Index returns BOC/V39065 CAD <- Quandl("BOC/V39065", start_date=year, collapse="daily")
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

Er.M <- 100*(mean(NYSE.returns[,2],na.rm = T))

NYSE.returns <- merge(NYSE.returns,CAD.returns, by="date" )

NYSE.returns <- NYSE.returns  %>%
  mutate(rate.change = (rate.NYSE - rate.CAD)) %>%
  select(-rate.NYSE,-rate.CAD)

#NYSE.returns <- NYSE.returns[-1,]


# iterate over each stock in wd
capm <- foreach(i = 1:length(list.files("Stock.out17/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out17", list.files("Stock.out17/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  read_data <- merge(read_data,CAD.returns, by='date')
  read_data <- read_data %>% 
    mutate(rate.x = (rate - rate.CAD) ) %>% 
    select(-rate,rate.CAD )
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(NYSE.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate.x ~ rate.change, na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out17/"),alpha = ldply(capm[,2])[,2], beta = ldply(capm[,3])[,2])
beta.table1 <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))
tickers <- as.character(beta.table1$ticker)
require(jsonlite)
metrics <- getQuote(tickers)
metrics$ticker <- metrics$symbol
#metrics <- cbind(ticker = rownames(metrics), metrics)

beta.table1 <- beta.table1 %>% 
 group_by(ticker) %>% 
  mutate(CAPM = 100*( rf  + (beta*((Er.M - rf))))) 

beta.table1[,c(2:7)] <- round(beta.table1[,c(2:7)], digits = 2)

#rm = [(1+R)^(1/n)]-1
#aaa <- (1+a)^(1/(365))-1

#beta.table1 <- beta.table1 %>% 
# group_by(ticker) %>% 
#  mutate(CAPM = rf  + (beta*((mean*100 - rf)))) 
#*********************** Metrics **********************************

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

#colnames(metrics)[4] <- "Market.Capitalization"


#metric_conversion <- marketString_to_numeric(metrics)
vec <- 3:27
for(i in 1:length(vec)) {
  metric_conversion[,vec[i]] <- as.numeric(metric_conversion[,vec[i]])
}


#metric_table <- data.frame(ticker = rownames(metric_conversion), metric_conversion)
#metric_table <- data.frame(ticker = Symbols,ldply(StockDat[,3])[,-1][,-1])
#library(stringdist)
#d <- expand.grid(beta.table$ticker,metric_conversion$ticker) # Distance matrix in long form
#names(d) <- c("a_name","b_name")
#d$dist <- stringdist(d$a_name,d$b_name, method="jw") # String edit distance (use your favorite function here)

#Greedy assignment heuristic (Your favorite heuristic here)
#greedyAssign <- function(a,b,d){
#  x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
# 1 for already assigned, -1 for unassigned and unassignable
#  while(any(x==0)){
#    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
#    a_sel <- a[d==min_d & x==0][1] 
#    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
#    x[a==a_sel & b == b_sel] <- 1
#    x[x==0 & (a==a_sel|b==b_sel)] <- -1
#  }
#  cbind(a=a[x==1],b=b[x==1],d=d[x==1])
#}
#dist <- data.frame(greedyAssign(as.character(d$a_name),as.character(d$b_name),d$dist))
#dist <- dist[,c(1,2)]
#colnames(dist)[2] <- "ticker"
#dataTable.1 <- merge(dist, beta.table, by.x=c("a"), by.y=c("ticker"), all = TRUE)
companies <- inner_join(beta.table1, metrics, by= "ticker")
#str(companies)

# VaR calculation
az <- 1000          # Number of stocks
# Value of portfolio
hp <- 1             # Holding period
a  <- .95           # Confidence level (5%)


#https://www.r-bloggers.com/calculating-var-with-r/
datatable <- companies %>% mutate(Parametric.VaR = abs((regularMarketPrice*az)*qnorm(1-a,0,1)*sd*sqrt(hp)))
datatable.4 <- datatable %>% mutate(Historical.VaR = abs(quantile(mean*100,1-a)*regularMarketPrice*az))

###### Stock Market Industries
NYSE.Sector <- read.csv("Ticker_Files/NYSE_Sector.06.24.2016.csv")
NASDAQ.Sector <- read.csv("Ticker_Files/NASDAQ_Sectors.06:24:2016.csv")
TSX.Sector <- read.csv("Ticker_Files/TSX_Industry.names.csv")

####### FInd the right industries  or skip this part and use b
Sector <- TSX.Sector


#--------------------------------------------------------------- Sectors
a <- datatable.4
b <- Sector

#a$name <- as.character(a$name)
#b$name <- as.character(b$Name)
# Escarpe everything after gsub("\\..*","",a)
a$ticker.1  <- a$ticker
a$ticker.1 <- gsub('\\.TO*', '', a$ticker.1)
a$ticker.1 <- gsub('\\-.*', '', a$ticker.1)
a$ticker.1 <- gsub('\\..*', '', a$ticker.1)

#dt3 <- a %>%
#  left_join(b, by = c( "ticker" = "ticker.1")) %>% 
#  select(-ticker.1)


comparables <- merge(a,b, by.x="ticker.1", by.y="ticker")
comparables <- comparables[,-c(1,76)]

#View(datatable.4)

comparables <- comparables[!duplicated(comparables$ticker),]
datatable.4 <- datatable.4[!duplicated(datatable.4$ticker),]





#companies <- companies[-2]
#dataTable <- as.data.frame(inner_join(beta.table, metric_conversion, by = "ticker"))

#data_output/stock_stats_MX-Sim_10.04.2016.csv
#trendPlots/Other.tickers/US_Undervalued.09.26.2016_data.csv
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(comparables,"data_output/stock_stats_MX-Sim_2017.11.17.csv")
#__________________________________________________________________


library(beepr)
beep(4)
