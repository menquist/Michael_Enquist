library(shiny)
library(quantmod)
library(rJava)
library(xlsx)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version

symbols <- tickers
symbols <- c('MMM','AXP','AAPL','BA','CAT','CVX','CSCO','KO','DD','XOM','GE','GS','HD','INTC','IBM','JNJ','JPM','MCD','MRK','MSFT','NKE','PFE','PG','TRV','UNH','UTX','VZ','V','WMT','DIS')

ratios <- c("Ask","Ave. Daily Volume","Ask Size","Bid","Ask (RT)","Bid (RT)","Book Value","Bid Size","Change & % Change","Change","Commission","Change (RT)","After Hours Change (RT)","Dividend/Share","Last Trade Date","Trade Date","Earnings/Share","Error Indication (returned for symbol changed / invalid)","EPS Estimate Current Year","EPS Estimate Next Year","EPS Estimate Next Quarter","Float Shares","Low","High","52-week Low","52-week High","Holdings Gain %","Annualized Gain","Holdings Gain","Holdings Gain % (RT)","Holdings Gain (RT)","More Info","Order Book (RT)","Market Capitalization","Market Cap (RT)","EBITDA","Change From 52-week Low","% Change From 52-week Low","Last Trade (RT) With Time","%Change (RT)","Last Size","Change From 52-week High","% Change From 52-week High","Last","Last","High Limit","Low Limit","Days Range","Days Range (RT)","50-day MA","200-day MA","Change From 200-day MA","% Change From 200-day MA","Change From 50-day MA","% Change From 50-day MA","Name","Notes","Open","P. Close","Price Paid","% Change","Price/Sales","Price/Book","Ex-Dividend Date","P/E Ratio","Dividend Pay Date","P/E Ratio (RT)","PEG Ratio","Price/EPS Estimate Current Year","Price/EPS Estimate Next Year","Symbol","Shares Owned","Short Ratio","Last Trade Time","Trade Links","Ticker Trend","1 yr Target Price","Volume","Holdings Value","Holdings Value (RT)","52-week Range","Days Value Change","Days Value Change (RT)","Stock Exchange","Dividend Yield")
setwd("~/R_Stuff/Stock_Analysis/Stock.out15")
#get statements for all stocks and write to xlsx
for (i in 1:length(symbols)) {
  getSymbols(symbols[i],src="google")
  temp <- getFinancials(symbols[i],src="google",auto.assign=FALSE)
  write.xlsx(temp$IS$A,paste(tolower(symbols[i]),"_IS.csv",sep=""))
  write.csv(temp$BS$A,paste(tolower(symbols[i]),"_BS.csv",sep=""))
  write.csv(temp$CF$A,paste(tolower(symbols[i]),"_CF.csv",sep=""))
}

#read statements for each stock & assign to corresponding dataframes
for (i in 1:length(symbols)) {
  assign(paste(tolower(symbols[i]),"_BS",sep=""),read.xlsx(paste(tolower(symbols[i]),"_BS.xlsx",sep=""),sheetName="Sheet1"))
  assign(paste(tolower(symbols[i]),"_IS",sep=""),read.xlsx(paste(tolower(symbols[i]),"_IS.xlsx",sep=""),sheetName="Sheet1"))
  assign(paste(tolower(symbols[i]),"_CF",sep=""),read.xlsx(paste(tolower(symbols[i]),"_CF.xlsx",sep=""),sheetName="Sheet1"))
}
#fetch ratios for all symbols
#for (i in 1:length(symbols)) {
#	assign(paste(tolower(symbols[i]),"_ratios",sep=""), getQuote(symbols[i], what=yahooQF(ratios)))
#	}

#get last 3 chars of string
rtSub <- function(string){
  subStr = substr(string,nchar(string)-2,nchar(string))
  return(subStr)
}

#wholesale renaming of columns of all dataframes that hold financial statements
for (strings in ls()) {
  if((rtSub(strings) == "_BS") | (rtSub(strings) == "_IS") | (rtSub(strings) == "_CF")) {
    df.tmp <- get(strings)
    names(df.tmp) <- c("Particulars","2017","2016","2015","2014")
    assign(strings,df.tmp)
  }}
