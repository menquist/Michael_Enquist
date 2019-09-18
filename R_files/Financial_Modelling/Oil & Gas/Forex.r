
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
  library(rjson)
  library(rCharts)
  library(Quandl)
  source("StockType_Function.R")
})



getFX("USD/AUS", from = "2000-01-01")
getFX("USD/EUR", from = "2015-01-01")
getFX("USD/CDN", from = "2015-01-01")
getFX("USD/JPY", from = "2015-01-01")
getFX("USD/AUD", from = "2015-01-01")
getFX("USD/CHF", from = "2015-01-01")
getFX("USD/CRC", from = "2015-01-01")
getFX("USD/MXN", from = "2015-01-01")
getFX("USD/GBP", from = "2015-01-01")



######## Create Currency Data Frame
usaus <- data.frame(time=time(USDAUS), USAUS = as.numeric(USDAUS))
usgbp <- data.frame(time=time(USDGBP), USGBP = as.numeric(USDGBP))
useur <- data.frame(time=time(USDEUR), USEUR = as.numeric(USDEUR))
usjpy <- data.frame(time=time(USDJPY), USJPY = as.numeric(USDJPY))
uschf <- data.frame(time=time(USDCHF), USCHF = as.numeric(USDCHF))
uscrc <- data.frame(time=time(USDCRC), USCRC = as.numeric(USDCRC))
usmxn <- data.frame(time=time(USDMXN), USMXN = as.numeric(USDMXN))
uscad <- data.frame(time=time(USDCDN), USDCDN = as.numeric(USDCDN))

ALL_CURRENCY <- usaus %>% 
  inner_join(usgbp, by = "time") %>% 
  inner_join(useur, by = "time") %>% 
  inner_join(usjpy, by = "time") %>% 
  inner_join(uschf, by = "time") %>% 
  inner_join(uscrc, by = "time") %>%
  inner_join(usmxn, by = "time") %>% 
  inner_join(uscad, by = "time")


pairs(ALL_CURRENCY)


################  Mutate FUNCTIONs
CURRENCY_rates <- usaus %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USAUS = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")

CURRENCY_rates1 <- usgbp %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USGBP = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")

CURRENCY_rates2 <- useur %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USEUR = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")


CURRENCY_rates3 <- usjpy %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USJPY = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")

CURRENCY_rates4 <- uschf %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USCHF = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")

CURRENCY_rates5 <- uscrc %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USCRC = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")

CURRENCY_rates6 <- usmxn %>%
  melt(id.vars = "time") %>%
  group_by(variable) %>%
  mutate(rate.USMXN = (value- lag(value))/lag(value)) %>%
  select(-value) %>%
  filter(time != "2015-01-01")



################################ Add all Currencies into one dataframe 
ALL_CURRENCY <- CURRENCY_rates %>% 
  inner_join(CURRENCY_rates1, by = "time") %>% 
  inner_join(CURRENCY_rates2, by = "time") %>% 
  inner_join(CURRENCY_rates3, by = "time") %>% 
  inner_join(CURRENCY_rates4, by = "time") %>% 
  inner_join(CURRENCY_rates5, by = "time") %>%
  inner_join(CURRENCY_rates6, by = "time") 
  

################## GGPlot I havent started it yet!!!
ggplot(ALL_CURRENCY, aes(, fill = time)) + 
  geom_density(alpha = 0.5)


df.melted <- melt(ALL_CURRENCY, id.vars = "time")
  
ggplot(df.melted, aes(x=time, y= value, color = variable)) + geom_point()



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
Symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/FX.csv", quote="\"", stringsAsFactors=FALSE)$V1

# create empty Symbol vector
Symbols_empty <- rep(0, length(Symbols1)) 

arg <- commandArgs(TRUE)
Symbols <- fun(IndexType = "NYSE", Symbols1 = Symbols1, Symbols = Symbols_empty)


# vector of performance metrics
perform_vec <- c( "")
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
    getSymb <- try(getFX(
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
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out19", StockDat[,4][[i]], sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}
