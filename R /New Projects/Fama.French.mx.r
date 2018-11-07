
########################################################################
########################### THE STOCK PROGRAM ##########################
###########################   ########################

########################################################################

aa <- as.factor(a[,c(1)])
aa <- data.frame(0,aa)
colnames(aa)[2] = 'symbols'

bb <- as.factor(b[,c(1)])
bb <- data.frame(0,bb)
colnames(bb)[2] = 'symbols'

symbols <- data.frame(0,symbols)
symbols$ID <- 1:nrow(symbols)
symbols$symbols <- as.factor(symbols$symbols)


library(dplyr)
symbols =anti_join(symbols, aa, by="symbols")
symbols =anti_join(symbols, bb, by="symbols")


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

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here

#-----------------------------------------

# Plug ticker in
sdfsdffjsdlkfjkl
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------

library(beepr)

# Read in ticker symbols by using the NASDAQ.csv file
symbols <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NASDAQ.csv", quote="", stringsAsFactors=FALSE)$V1
#symbols1 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/NYSE_entire_complist.csv", quote="", stringsAsFactors=FALSE)
#symbols2 <- read.table("~/R_Stuff/Stock_Analysis/Ticker_Files/TSX.csv", quote="", stringsAsFactors=FALSE)
#symbols <-   !symbols !== aa 


#symbols$symbols <- gsub(aa, "",symbols$symbols)



#symbols$symbols <- as.character(symbols$symbols)
#datNew1 <- droplevels( symbols[-which(symbols$symbols == aa), ] )
#xx <- subset(symbols, select=-c(aa))
#xx <-  symbols[ , -which(names(symbols) %in% c("z","u"))]
#symbols <-   symbols[!symbols$symbols == aa,] 
#symbols <-   symbols[!symbols$symbols == bb,] 

#dtfm[!dtfm$C == "Foo", ]
#symbols <- rbind_list(symbols,symbols1)

#symbols <- symbols$V1[1:100]

#symbols <- as.character(symbols$symbols)



perform_vec <- c( "Earnings/Share", "Market Capitalization","EBITDA", "Book Value",
                  "Dividend/Share", "EPS Estimate Current Year", "EPS Estimate Next Year", 
                  "EPS Estimate Next Quarter", "Float Shares", "52-week Low", "52-week High",
                  "Last Trade (RT) With Time", "Price/Sales", "Price/Book","P/E Ratio", 
                  "PEG Ratio", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
                  "Short Ratio", "1 yr Target Price" , "Dividend Yield","Volume", "Ask", "Bid", "Open", 
                  "Last Trade (Price Only)","Name","Ex-Dividend Date")




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
rm(list=setdiff(ls(), c("dataset", "perform_vec","symbols","year","getQuote")))





dat1 <- as.data.frame(dataset)
nr <- nrow(dat1)
dat1 <- (dat1[-1,] - dat1[-nr,]) / dat1[-nr,]

str(dat1)

library(functional)

do.call(data.frame,lapply(dat1, function(x) replace(x, is.infinite(x),NA)))




dat  <- data.frame(date=round(index(dataset[-1,],"day")), coredata(dat1))
dat <- cbind(ticker = rownames(dat), dat)
rownames(dat) <- 1:nrow(dat)
date  <- dat[,2]
dat <- dat[,-c(1,2)]

date <- round(date,"days")


dat <- dat[,colSums(is.na(dat))<nrow(dat)]

str(dat)


stockname <- colnames(dat)

stockname <-gsub(".Adjusted", "", stockname)
#data <- dat[100:200]
test <- sapply(dat1, function(col) ifelse(col>0.01,1,ifelse(col<=0.01,0,col)))
test <- data.frame(test)

xx 

for(i in 1:length(stockname)) {
  if(exists("dat")) {
    Stock_df <- data.frame( date = date, rate = test[[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out17", stockname[i],sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}


for(i in 1:length(stockname)) {
  if(exists("dat")) {
    Stock_df <- data.frame( date = date, rate = dat[[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out16", stockname[i],sep = "/"))
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

#check nan inf
#which(is.na(Volatility_Table), arr.ind=TRUE)
a<-  as.data.frame(Volatility_Table[is.na(Volatility_Table$sd),]) 
#a
#remove files in folder
#setwd("~/R_Stuff/Stock_Analysis/Stock.out17")
file.removed <- as.character(a[,1])
#str(file.removed)

#### sum = 1


#check nan inf
#which(is.na(Volatility_Table), arr.ind=TRUE)
b <- as.data.frame(Volatility_Table[ Volatility_Table$sum < 5,]) 
#b
#remove files in folder

file.removed.1 <- as.character(b[,1])
str(file.removed.1)
Sys.sleep(3)
setwd("~/R_Stuff/Stock_Analysis/Stock.out17/")
#Sys.sleep(3)

#mydir <- "Stock.out17/"
junk <- dir(path="Stock.out17/", pattern=file.removed) # ?dir
file.remove(junk) # ?file.remove#
#setwd("~/R_Stuff/Stock_Analysis")
Sys.sleep(3)
file.remove(aa)
Sys.sleep(3)
setwd("~/R_Stuff/Stock_Analysis/Stock.out17/")
file.remove(file.removed)
mydir <- "Stock.out17/"
delfiles <- dir(path=mydir ,pattern= file.removed.1)
file.remove(file.path(mydir, delfiles))

junk <- dir(path="Stock.out17/", pattern=file.removed.1) # ?dir
file.remove(junk) # ?file.remove#
Sys.sleep(3)
file.remove(file.removed.1)
setwd("~/R_Stuff/Stock_Analysis/")

Sys.sleep(4)

Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
Volatility_Table <- na.omit(Volatility_Table)

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

#check nan inf
#which(is.na(Volatility_Table), arr.ind=TRUE)
c<-  as.data.frame(Volatility_Table[is.na(Volatility_Table$sd),]) 
#a
#remove files in folder
setwd("~/R_Stuff/Stock_Analysis/Stock.out16/")
file.removed.2 <- as.character(c[,1])
str(file.removed.2)
Sys.sleep(3)

junk <- dir(path="Stock.out16/", pattern=file.removed.2) # ?dir
file.remove(junk) # ?file.remove#
Sys.sleep(3)
file.remove(file.removed.2)
setwd("~/R_Stuff/Stock_Analysis/Stock.out17/")
Sys.sleep(4)

file.remove(file.removed.2)


setwd("~/R_Stuff/Stock_Analysis/")
Sys.sleep(4)


Sys.sleep(3)

junk <- dir(path="Stock.out17/", pattern=file.removed) # ?dir
file.remove(junk) # ?file.remove#
setwd("~/R_Stuff/Stock_Analysis")
Sys.sleep(4)



Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
Volatility_Table <- na.omit(Volatility_Table)


# get data
fromDate <- year


oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

XOI.NYSE <- getSymbols("^XOI", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

ZEO.BMO <- getSymbols("XLK", src = "yahoo", from = fromDate, 
                      auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

GSPC <- getSymbols("^GSPC", src = "yahoo", from = fromDate, 
                   auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
VIX <- getSymbols("^VIX", src = "yahoo", from = fromDate, 
                  auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]


HK <- getSymbols("^HSI", src = "yahoo", from = fromDate, 
                  auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

XOI.returns <- data.frame(date = as.factor(time(XOI.NYSE)), price = as.numeric(XOI.NYSE))  %>%
  mutate(rate.XOI = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

ZEO.returns <- data.frame(date = as.factor(time(ZEO.BMO)), price = as.numeric(ZEO.BMO))  %>%
  mutate(rate.ZEO = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

# get Index returns
GSPC.returns <- data.frame(date = as.factor(time(GSPC)), price = as.numeric(GSPC))  %>%
  mutate(rate.GSPC = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

# get Index returns

VIX.returns <- data.frame(date = as.factor(time(VIX)), price = as.numeric(VIX))  %>%
  mutate(rate.VIX = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)
#cad.returns

China.returns <- data.frame(date = as.factor(time(HK)), price = as.numeric(HK))  %>%
  mutate(rate.China = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)


library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")


CAD <- Quandl("CUR/CAD", start_date=year, collapse="daily")
GOLD <- Quandl("LBMA/GOLD", start_date=year, collapse="daily")
### Cost to register property (% of property value) - Hong Kong SAR, China

#OPEC/ORB FED/RXI_N_B_CH
China <- Quandl("FED/RXI_N_B_CH", start_date=year, collapse="daily")

#ggplot(OPEC, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
#ggplot(GOLD, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
#colnames(Percent.of.property.value.HK)[2] <- c("Cost to register property (% of property value)") 
CAD.USA.EX <- Quandl("BOE/XUMLCDD",collapse="daily", start_date=year)
#CAD.US <- getSymbols("DEXCAUS", src="FRED", auto.assign = FALSE)
CAD.new <- getSymbols("CADUSD=X", src = "yahoo", from = year, 
                      auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

#

CAD <- CAD %>% 
  group_by(DATE) %>% 
  mutate(rate = 1/RATE) %>% 
  select(-RATE)
colnames(CAD)[1] <- "date"  
colnames(CAD.USA.EX)[1] <- colnames(CAD)[1] 
colnames(CAD.USA.EX)[2] <- colnames(CAD)[2] 
colnames(China)[1] <- colnames(CAD)[1] 
colnames(China)[2] <- colnames(CAD)[2] 


dat  <- data.frame(date=round(index(CAD.new[,1],"day")), coredata(CAD.new))
colnames(dat) <- c("date", "rate")

library(data.table)

#library(Quandl)
#Quandl.api_key("c58SRnkLsYYrxJs6d74e")


CAD.USA.EX <- rbindlist(list(CAD.USA.EX,dat,CAD), fill=T)
CAD.USA.EX <- CAD.USA.EX[!duplicated(CAD.USA.EX$date),]

CAD.USA.EX <- as.data.frame(CAD.USA.EX)
#OPEC <- OPEC[rev(rownames(OPEC)),]
China <- China[rev(rownames(China)),]
GOLD <- GOLD[rev(rownames(GOLD)),]
CAD.USA.EX <- CAD.USA.EX[rev(rownames(CAD.USA.EX)),]
GOLD <- GOLD %>% 
  select(Date,"USD (AM)")

colnames(GOLD)[1] <- colnames(CAD.USA.EX)[1]

#OPEC <- xts(OPEC[,-1], order.by=as.Date(OPEC[,1], "%m/%d/%Y"))
China <- xts(China[,-1], order.by=as.Date(China[,1], "%m/%d/%Y"))
GOLD <- xts(GOLD[,-1], order.by=as.Date(GOLD[,1], "%m/%d/%Y"))
CAD.USA.EX <- xts(CAD.USA.EX[,-1], order.by=as.Date(CAD.USA.EX[,1], "%m/%d/%Y"))

#dataset <- xts(CAD.USA.EX)

#
#  daily returns
#
#returns <- OPEC[diff(OPEC$Value, arithmetic=FALSE ) - 1,]


#revdf$Date <- as.Date(as.character(revdf$Date),format="%Y%m%d")
# create xts object
#x <- xts(revdf$Value,revdf$Date)

#dat1 <- (OPEC[-1,] - OPEC[-nr,]) / OPEC[-nr,]




# get Index returns
#China.returns <- data.frame(date = as.factor(time(China)), price = as.numeric(China))  %>%
#  mutate(rate.China = (price - lag(price))/lag(price)) %>%
#  filter(date != year)  %>% 
#  select(-price)

# get Index returns

Gold.returns <- data.frame(date = as.factor(time(GOLD)), price = as.numeric(GOLD))  %>%
  mutate(rate.GOLD = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

# get Index returns
CAD.returns <- data.frame(date = as.factor(time(CAD.USA.EX)), price = as.numeric(CAD.USA.EX))  %>%
  mutate(rate.CAD = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)



data.returns<- Reduce(merge,list(VIX.returns,NYSE.returns,XOI.returns,ZEO.returns,GSPC.returns,Gold.returns,CAD.returns,China.returns))

x <- test
y <- data.returns
y1 <- na.omit(data.returns)
data.returns <- na.omit(data.returns)
all(is.na(x))
all(is.na(y))
#all(is.na(y^trans))

FAMA <- foreach(i = 1:length(list.files("Stock.out17/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out17", list.files("Stock.out17/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  #read_data <- na.omit(read_data)
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(data.returns, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  
  # regress returns
  mod <- lm(rate ~ rate.GOLD+rate.NYSE+rate.XOI+rate.ZEO+rate.GSPC+rate.GOLD+rate.CAD+rate.China+rate.VIX, na.action=na.omit, data = joined.ret)
  intercept <- mod$coefficients[1]
  beta.gold <- mod$coefficients[2]
  beta.nyse <- mod$coefficients[3]
  beta.xoi <- mod$coefficients[4]
  beta.zeo <- mod$coefficients[5]
  beta.gspc <- mod$coefficients[6]
  beta.cad <- mod$coefficients[7]
  beta.china <- mod$coefficients[8]
  beta.vix <- mod$coefficients[9]
  #beta.vix <- mod$coef[10]
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta.nyse,beta.gold = beta.gold, beta.xoi = beta.xoi,
              beta.zeo = beta.zeo, beta.gspc = beta.gspc, beta.china=beta.china,beta.vix=beta.vix, beta.cad=beta.cad,
              rate = joined.ret[,3], date = joined.ret$date))
}

#data <- na.omit(data) CAD.returns beta.cad = beta.cad
#data.returns<- Reduce(merge,list(Gold.returns,NYSE.returns,XOI.returns,ZEO.returns,OPEC.returns))

#FAMA.log <- foreach(i = 1:length(list.files("Stock.out18/")), .combine = rbind) %do% {

# gather data
#  read_data <- read.csv(paste("Stock.out18", list.files("Stock.out18/")[i], sep = "/"))[,-1]
#  read_data$date <- as.factor(read_data$date)
#read_data <- na.omit(read_data)
# join index and data
#  options(warn=-1) 
#  joined.ret <- inner_join(data.returns, read_data, by = 'date')
#  joined.ret$date <- as.factor(joined.ret$date)
##joined.ret <- filter(joined.ret, rate != as.numeric(0))
#  joined.ret <- na.omit(joined.ret)
# joined.ret[is.na(joined.ret$rate), "rate"] <- 0
# logistic regression
#  mod <- glm(rate ~ rate.GOLD+rate.NYSE+rate.XOI+rate.ZEO+rate.OPEC+rate.CAD, data = joined.ret, family = "binomial")
#  intercept <- mod$coefficients[1]
#  beta.gold <- mod$coefficients[2]
#  beta.nyse <- mod$coefficients[3]
#  beta.xoi <- mod$coefficients[4]
#  beta.zeo <- mod$coefficients[5]
#  beta.opec <- mod$coefficients[6]
#  beta.cad <- mod$coefficients[7]

# regress returns
#mod <- lm(rate ~ ., na.action=na.omit, data = joined.ret)
#intercept <- mod$coef[1]
#beta.gold <- mod$coef[2]
#beta.nyse <- mod$coef[3]
#beta.xoi <- mod$coef[4]
#beta.zeo <- mod$coef[5]
#beta.opec <- mod$coef[6]
#beta.cad <- mod$coef[7]
# return int, beta and data
#  return(list(model = mod, int = intercept, beta = beta.nyse,beta.gold = beta.gold, beta.xoi = beta.xoi,
#              beta.zeo = beta.zeo, beta.opec = beta.opec, beta.cad = beta.cad,
#              rate = joined.ret[,3], date = joined.ret$date))
#}



# output table of stock beta.gold beta.cad= ldply(FAMA[,8])[,2] beta.china= ldply(FAMA[,1])[,2])

beta.df <- data.frame(ticker = list.files("Stock.out17/"),alpha = ldply(FAMA[,2])[,2], beta.nyse = ldply(FAMA[,3])[,2], beta.opec= ldply(FAMA[,4])[,2],
                      beta.xoi= ldply(FAMA[,5])[,2], beta.zeo= ldply(FAMA[,6])[,2], beta.gspc = ldply(FAMA[,7])[,2], beta.gold = ldply(FAMA[,8])[,2],beta.cad= ldply(FAMA[,9])[,2],
                      beta.vix= ldply(FAMA[,10])[,2] )

#beta.vix= ldply(FAMA[,11])[,2]
# output table of stock beta.gold logistic reg
beta.df1 <- data.frame(ticker = list.files("Stock.out18/"),alpha = ldply(FAMA.log[,2])[,2], beta.nyse = ldply(FAMA.log[,3])[,2], beta.opec= ldply(FAMA.log[,4])[,2],
                       beta.xoi= ldply(FAMA.log[,5])[,2], beta.zeo= ldply(FAMA.log[,6])[,2], beta.gold= ldply(FAMA.log[,7])[,2],
                       beta.cad= ldply(FAMA.log[,8])[,2])

#ALPHA.df <- data.frame(ticker = list.files("Stock.out17/"), A = ldply(capm[,2])[,2], A1= ldply(capm.OPEC[,2])[,2],
#                      A2= ldply(capm.XOI[,2])[,2], A3= ldply(capm.ZEO[,2])[,2],A4= ldply(capm.GOLD[,2])[,2] ,A5= ldply(capm.CAD[,2])[,2])
#ALPHA.df <- ALPHA.df %>% 
#  group_by(ticker) %>% 
#  mutate(alpha = A + A1 + A2 + A3 + A4 + A5) 

#ALPHA.df <- ALPHA.df %>% 
#  select(ticker,alpha)

#beta.df <- as.data.frame(inner_join(beta.df, ALPHA.df, by = 'ticker'))
#str(beta.df)

beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))
beta.table1 <- as.data.frame(inner_join(Volatility_Table, beta.df1, by = 'ticker'))


beta.table.test <- beta.table1 %>% 
  group_by(ticker) %>% 
  mutate(FAMA =  alpha + beta.nyse + beta.opec+beta.xoi+beta.zeo+beta.gold+beta.cad) 

beta.table.oil1 <- beta.table1 %>% 
  group_by(ticker) %>% 
  mutate(FAMA = .02 + alpha + (beta.nyse*(mean - .02)) + beta.opec+beta.xoi+beta.zeo+beta.gold+beta.cad) 

beta.table.oil <- beta.table %>% 
  group_by(ticker) %>% 
  mutate(FAMA = .02 + alpha + (beta.nyse*(mean - .02)) +beta.opec+beta.xoi+beta.zeo+beta.gspc+beta.gold+beta.cad) 

#beta.table.oil1 <- beta.table %>% 
#  group_by(ticker) %>% 
#  mutate(FAMA = .02 + alpha + (beta.nyse*(mean - .02)) + (3*sd)+(beta.opec+beta.xoi+beta.zeo+beta.gspc+beta.gold+beta.cad)) 


###### Stock Market Industries
NYSE.Sector <- read.csv("Ticker_Files/NYSE_Sector.06.24.2016.csv")
#NASDAQ.Sector <- read.csv("Ticker_Files/NASDAQ_Sectors.06:24:2016.csv")
TSX.Sector <- read.csv("Ticker_Files/TSX_Industry.names.csv")

library(data.table)

l <- rbind_list(NYSE.Sector,TSX.Sector)


l$ticker <- as.character(l$ticker)

library(beepr)
match <- inner_join(beta.table.oil,l, by="ticker") 

DATE <- Sys.Date()
DATE
match$Date <- DATE
beep(4)
write.csv(match, "data_output/FAMA-2018.08.03.csv")
#write.csv(data.returns, "data_output/data.returns-2018.06.02.csv")
