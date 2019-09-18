
tickers <-new.env()
t <-c("AAL",  "AAME", "AAOI")
t

lapply(t, getFinancials)
BS <- data.frame(lapply(tickers, function(x) {viewFinancials(x, type= 'BS', period = 'A')}))
IS <- data.frame(lapply(tickers, function(x) {viewFinancials(x, type= 'IS', period = 'A')}))
CF <- data.frame(lapply(tickers, function(x) {viewFinancials(x, type= 'CF', period = 'A')}))

BS

financials <- getFinancials("HKG:0005", auto.assign=F,period="A")
viewFinancials(financials, type="BS", period="A")
viewFinancials(financials, type="IS", period="Q")
viewFinancials(financials, type="CF", period="Q")

get_financials_df <- function(symbol, type, period = 'A') {
  temp.data <- quantmod::viewFinancials(quantmod::getFinancials(symbol, src = 'google', env = NULL), 
                                        type = type,
                                        period = period
  )
  
  temp.data <- data.table::setDT(as.data.frame(temp.data),
                                 keep.rownames=TRUE)
  
  temp.data <- data.table::setnames(temp.data, "rn", "account")
  
  temp.data <- data.table::melt(temp.data, id.vars = 'account',
                                variable.name = 'date',
                                value.name = 'value'
  )
  
  temp.data <- temp.data[, index := .GRP, by = list(account)]
  
  temp.data <- temp.data[, date_parse := lubridate::parse_date_time(date, '%Y-%m-%d'),]
  
  temp.data <- temp.data[order(-temp.data$date_parse , temp.data$index)]
  
  temp.data[is.na(temp.data)] <- 0
  
  temp.data
}

BS <- data.frame(lapply(tickers, function(x) {get_financials_df(x, type= 'BS', period = 'A')}))

get_financials_df(symbol = AAPL,type = "BS",period = "A")




##################
#Import Constituents List
#constituents <- read.csv("/Users/marsh/Desktop/russel_2000_constituents.csv")
library(quantmod)
library(dplyr)
library(jsonlite)
library(httr)

constituents <- as.character(Volatility_Table$ticker[1:10])

#Extract tickers from constituents list and convert to "financials" class
tickers <- constituents
adj_tickers <- NULL
for(i in tickers) {
  adj_ticker <-  try(getFin(i))
  adj_tickers <- c(adj_tickers, adj_ticker)
  print("Processing...please wait...")
}

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
    #NCAVPS <- (balance_sheet[10,1] - balance_sheet[31,1])/balance_sheet[42,1]
    df <- cbind(i, NCAVPS)
    multiple_NCAVPS <- rbind(multiple_NCAVPS, df)
  }
}


tickers <- new.env()
t <- c("PHG", "AAPL")
t <- as.character(Volatility_Table$ticker[1:50])






lapply(adj_tickers, getFin, env = tickers)
BS <-
  data.frame(lapply(tickers, function(x) {
    viewFinancials(x, type = 'BS', period = 'A')
  }))
IS <-
  data.frame(lapply(tickers, function(x) {
    viewFinancials(x, type = 'IS', period = 'A')
  }))
CF <-
  data.frame(lapply(tickers, function(x) {
    viewFinancials(x, type = 'CF', period = 'A')
  }))

financials <- do.call(bind_rows, lapply(adj_tickers, function(x) {
  dat <- NULL
  tryCatch(dat <- tq_get(x, get='financials') %>% mutate(symbol=x), error=function(e) {
    message('failed to get data for ', x)
  })
  dat
}))
