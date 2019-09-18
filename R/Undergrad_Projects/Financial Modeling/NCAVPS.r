#Import Constituents List
setwd("~/R_Stuff/Stock_Analysis")
library(quantmod)



library(dplyr)
library(jsonlite)
library(httr)
library(beepr)
constituents <- as.character(Volatility_Table$ticker[1:10])
#constituents <- read.table("Ticker_Files/oilcomptestlist.csv", quote="", stringsAsFactors=FALSE)$V1






#Extract tickers from constituents list and convert to "financials" class
tickers <- constituents
adj_tickers <- NULL
for(i in tickers) {
  adj_ticker <-  try(getFin(i))
  adj_tickers <- c(adj_tickers, adj_ticker)
  print("Processing...please wait...")
}


#adj_tickers <- gsub("",".f$", constituents)

#View most recent quarterly balance sheet of each company
multiple_NCAVPS <- NULL
for(i in adj_tickers) {
  if(grepl("Error", i) == TRUE) {
    print("Adjusted ticker not found...moving to next adjusted ticker...")
  } else {
    financial <- get(i)
    balance_sheet <-as.data.frame(viewFinancials(financial, type = "BS",
                                                 period = "Q"))
    #Current Assets are in row 10, total liabilities in row 31, total shares
    #outstanding in row 42
    NCAVPS <- (balance_sheet[10,1] - balance_sheet[31,1])/balance_sheet[42,1]
    df <- cbind(i, NCAVPS)
    multiple_NCAVPS <- rbind(multiple_NCAVPS, df)
  }
}

#Format multi_NCAVPS matrix to data frame
multiple_NCAVPS1 <- as.data.frame(multiple_NCAVPS)
multiple_NCAVPS <- rename(multiple_NCAVPS1, Company = i)
multiple_NCAVPS$NCAVPS <- as.numeric(as.character(multiple_NCAVPS$NCAVPS))
multiple_NCAVPS$Company <- as.character(multiple_NCAVPS$Company)
multiple_NCAVPS$Company <- gsub(".f$","", multiple_NCAVPS$Company)

#Remove negative NCAVPS and NA's
multiple_NCAVPS <- filter(multiple_NCAVPS, is.na(multiple_NCAVPS$NCAVPS) == FALSE)
multiple_NCAVPS <- filter(multiple_NCAVPS, multiple_NCAVPS$NCAVPS >= 0)

#Order multiple_NCAVPS by NCAVPS in descending order
multiple_NCAVPS <- arrange(multiple_NCAVPS, desc(NCAVPS))

#Pull live quote from Yahoo Finance and append to data frame multiple_NCAVPS
stockquotes <- NULL
for(i in multiple_NCAVPS$Company) {
  stockquote <- getQuote(i)
  last_price <- stockquote$Last
  stockquotes <- c(stockquotes, last_price)
  print("Processing...please wait...")
}
multiple_NCAVPS <- mutate(multiple_NCAVPS, Last_Price = stockquotes)
multiple_NCAVPS$Last_Price <- as.numeric(multiple_NCAVPS$Last_Price)
multiple_NCAVPS <- mutate(multiple_NCAVPS,
                          GrahamsRule = if_else(NCAVPS > Last_Price, 1, 0))

#Order multiple_NCAVPS by GrahamsRule
multiple_NCAVPS <- arrange(multiple_NCAVPS, desc(GrahamsRule))

#Add Margin of Safety attribute and order
multiple_NCAVPS <- mutate(multiple_NCAVPS,  price_per_dollar= Last_Price/NCAVPS)
multiple_NCAVPS <- arrange(multiple_NCAVPS, price_per_dollar)
multiple_NCAVPS
beep(3)

gQuote2 <- function(n){
  url <- paste('https://finance.google.com/finance/getprices?q=',n,'&p=10m&f=c&i=60',sep='')
  d <- tail(read.table(url,sep=',',header=F,skip=7),1)[[1]]
  return(d)
}



#' Get quotes from google (real-time) in json
library(jsonlite)
#' url <- paste('http://finance.google.com/finance/info?client=ig&q=',n,sep='')
gQuote <- function(n){
  
  url <- paste('https://finance.google.com/finance?q=',n,'&output=json',sep='')
  #print(url)
  data <- getURL(url)
  json <- fromJSON(gsub('\n|[//]|[\\]','',data))
  return(json)
}
a <- gQuote("AAPL")
