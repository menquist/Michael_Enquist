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
  library(XML)
  library(Quandl)
  registerDoMC(cores = 4)
})


# Be sure to set your working directory: set * to directory  example -> setwd("~/R_Stuff/Stock_Analysis/")

# Read in ticker symbols by using the NASDAQ.csv file
#stocks <- read.table("~/R_Stuff/Stock_Analysis/NYSE.Large.Cap.csv", quote="", stringsAsFactors=FALSE)$V1

#stocks <- stocks[1:50]


#Sample
stocks <- c("AAPL","BA","CAT","CSCO","IBM","AMZN","MSFT")


for (s in stocks) {
  url <- paste0("http://finviz.com/quote.ashx?t=", s)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
  # ASSIGN TO STOCK NAMED DFS
  assign(s, readHTMLTable(tableNodes[[9]], 
                          header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                    "data7", "data8", "data9", "data10", "data11", "data12")))
  
  # ADD COLUMN TO IDENTIFY STOCK 
  df <- get(s)
  df['stock'] <- s
  assign(s, df)
}

# COMBINE ALL STOCK DATA 
stockdatalist <- cbind(mget(stocks))
stockdata <- do.call(rbind, stockdatalist)
# MOVE STOCK ID TO FIRST COLUMN
stockdata <- stockdata[, c(ncol(stockdata), 1:ncol(stockdata)-1)]
colnames(stockdata) <- c("ticker","data", "data1", "data", "data1", "data", "data1",
                         "data", "data1", "data", "data1", "data", "data1" )



# SAVE TO CSV
#write.table(stockdata, "C:/Users/your_path_here/Desktop/MyData.csv", sep=",", 
#            row.names=FALSE, col.names=FALSE)


test1 <- data.frame(lapply(unique(names(stockdata)), function(x)
  setNames(data.frame(unlist(stockdata[names(stockdata)==x], use.names = FALSE)), x)))

Dataset <- reshape(test1, idvar = "ticker", timevar = "data", direction = "wide")
# remove ".x" and ".y" at the end of the country names
names(Dataset)  <- sub("data1.", "", names(Dataset))


# put object into CSV file
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(Dataset,"FINVIZ.csv")
#__________________________________________________________________





#data.env <- new.env()
dataset <- xts()
# cool progress bar to see the % of completion
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)


# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try()
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


stockquote <- <- function(ticks) {
  url <- paste0("http://finviz.com/quote.ashx?t=", s)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")