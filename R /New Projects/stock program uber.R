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
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)

# vector of performance metrics
perform_vec <- c("Earnings/Share", "Market Capitalization", 
                 "EBITDA","P/E Ratio", "Volume", "52-week Low", "52-week High", "Last Trade (Price Only)", "Name")

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
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out5", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}



#############################################################################!!!!!!!!!
########## Let's get a table of volatility and betas ########################
#############################################################################

#*********************** Volatility **********************************
Volatility_Table <- foreach(k = 1:length(list.files("Stock.out5/")), .combine = rbind) %do% {
  theSTOCK <- data.frame(read.csv(paste("Stock.out5", list.files("Stock.out5/")[k], sep = "/"))[,-1])
  return(list(mean = mean(theSTOCK[,2], na.rm = T), sd = sd(theSTOCK[,2], na.rm = T)))
}
Volatility_Table <- data.frame(ticker = list.files("Stock.out5/"), mean = ldply(Volatility_Table[,1])[,2], 
                               sd = ldply(Volatility_Table[,2])[,2])
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
capm <- foreach(i = 1:length(list.files("Stock.out5/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out5", list.files("Stock.out5/")[i], sep = "/"))[,-1]
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
beta.df <- data.frame(ticker = list.files("Stock.out5/"), beta = ldply(capm[,3])[,2])
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




#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(dataTable, "data_output/stock_stats_NYSE_Healthcare_Mar_11_2016.csv")
#__________________________________________________________________


