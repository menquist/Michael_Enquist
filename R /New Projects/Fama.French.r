library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")

year <- "1900-01-01"

CAD <- Quandl("CUR/CAD", start_date=year, collapse="daily")
GOLD <- Quandl("LBMA/GOLD", start_date=year, collapse="daily")
### Cost to register property (% of property value) - Hong Kong SAR, China

#OPEC/ORB
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
China.returns <- data.frame(date = as.factor(time(China)), price = as.numeric(China))  %>%
  mutate(rate.China = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

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

#check nan inf
#which(is.na(Volatility_Table), arr.ind=TRUE)
a<-  as.data.frame(Volatility_Table[is.na(Volatility_Table$sd),]) 
a
#remove files in folder
setwd("~/R_Stuff/Stock_Analysis/Stock.out13")
file.removed <- as.character(a[,1])
str(file.removed)
Sys.sleep(3)

junk <- dir(path="Stock.out13/", pattern=file.removed) # ?dir
file.remove(file.removed) # ?file.remove#
setwd("~/R_Stuff/Stock_Analysis")
Sys.sleep(4)

Volatility_Table <- filter(Volatility_Table, mean > as.numeric(0) | mean < as.numeric(0))
Volatility_Table <- as.data.frame(Volatility_Table)
Volatility_Table <- na.omit(Volatility_Table)


#*********************** Betas **********************************
# get data
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

XOI.NYSE <- getSymbols("XCS.TO", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]

ZEO.BMO <- getSymbols("XMD.TO", src = "yahoo", from = fromDate, 
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


#NYSE.returns <- NYSE.returns[-1,]


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

# iterate over each stock in OPEC
capm.OPEC <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(OPEC.returns, read_data, by = 'date')
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

# iterate over each stock in XOI
capm.XOI <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(XOI.returns, read_data, by = 'date')
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

# iterate over each stock in XOI
capm.ZEO <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(ZEO.returns, read_data, by = 'date')
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

# iterate over each stock in XOI
capm.GOLD <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(Gold.returns, read_data, by = 'date')
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

# iterate over each stock in XOI
capm.CAD <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(CAD.returns, read_data, by = 'date')
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

# iterate over each stock in XOI by = "i", all = TRUE)

data<- Reduce(merge,list(Gold.returns,NYSE.returns,XOI.returns,ZEO.returns,OPEC.returns,  CAD.returns))
#L <- list(Gold.returns,NYSE.returns,XOI.returns,ZEO.returns,OPEC.returns ,CAD.returns)
#merge.all <- function(x, y) {
#  merge(x, y,by='date',all =T )
#}
#output <- Reduce(merge.all, L)
#output <- output[!duplicated(output$date),]


#library(data.table)

#EXTRACT <- rbindlist(list(Gold.returns,NYSE.returns,XOI.returns,ZEO.returns,OPEC.returns ,CAD.returns), fill=T)
#EXTRACT <- EXTRACT[!duplicated(EXTRACT$date),]


FAMA <- foreach(i = 1:length(list.files("Stock.out13/")), .combine = rbind) %do% {
  
  # gather data
  read_data <- read.csv(paste("Stock.out13", list.files("Stock.out13/")[i], sep = "/"))[,-1]
  read_data$date <- as.factor(read_data$date)
  
  # join index and data
  options(warn=-1) 
  joined.ret <- inner_join(data, read_data, by = 'date')
  joined.ret$date <- as.factor(joined.ret$date)
  ##joined.ret <- filter(joined.ret, rate != as.numeric(0))
  
  # regress returns
  mod <- lm(rate ~ ., na.action=na.omit, data = joined.ret)
  intercept <- mod$coef[1]
  beta.gold <- mod$coef[2]
  beta.nyse <- mod$coef[3]
  beta.xoi <- mod$coef[4]
  beta.zeo <- mod$coef[5]
  beta.opec <- mod$coef[6]
  beta.cad <- mod$coef[7]
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta.nyse,beta.gold = beta.gold, beta.xoi = beta.xoi,
              beta.zeo = beta.zeo, beta.opec = beta.opec, beta.cad = beta.cad,
              rate = joined.ret[,3], date = joined.ret$date))
}



# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out13/"), beta = ldply(capm[,3])[,2], beta.opec= ldply(capm.OPEC[,3])[,2],
                      beta.xoi= ldply(capm.XOI[,3])[,2], beta.zeo= ldply(capm.ZEO[,3])[,2], beta.gold= ldply(capm.GOLD[,3])[,2],
                      beta.cad= ldply(capm.CAD[,3])[,2])

ALPHA.df <- data.frame(ticker = list.files("Stock.out13/"), A = ldply(capm[,2])[,2], A1= ldply(capm.OPEC[,2])[,2],
                      A2= ldply(capm.XOI[,2])[,2], A3= ldply(capm.ZEO[,2])[,2],A4= ldply(capm.GOLD[,2])[,2] ,A5= ldply(capm.CAD[,2])[,2])
ALPHA.df <- ALPHA.df %>% 
  group_by(ticker) %>% 
  mutate(alpha = A + A1 + A2 + A3 + A4 + A5) 

ALPHA.df <- ALPHA.df %>% 
  select(ticker,alpha)

beta.df <- as.data.frame(inner_join(beta.df, ALPHA.df, by = 'ticker'))
str(beta.df)

beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

beta.table.oil <- beta.table %>% 
  group_by(ticker) %>% 
  mutate(FAMA = .02 + alpha + (beta*(mean - .02)) + beta.opec+beta.xoi+beta.zeo+beta.gold+beta.cad) 

beta.table.oil <- beta.table.oil[!duplicated(beta.table.oil$ticker),]
tickers <- beta.table.oil$ticker
tickers <- as.character(tickers)
what_metrics <- yahooQF(perform_vec)
metrics <- getQuote(tickers, what=what_metrics)

# symbols
metrics <- cbind(ticker = rownames(metrics), metrics)
rownames(metrics) <- 1:nrow(metrics)

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

colnames(metrics)[4] <- "Market.Capitalization"


metric_conversion <- marketString_to_numeric(metrics)
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
companies <- inner_join(beta.table.oil, metric_conversion, by= "ticker")
#str(companies)

# VaR calculation
az <- 1000          # Number of stocks
# Value of portfolio
hp <- 1             # Holding period
a  <- .95           # Confidence level (5%)


#https://www.r-bloggers.com/calculating-var-with-r/
datatable <- companies %>% mutate(Parametric.VaR = abs((Last*az)*qnorm(1-a,0,1)*sd*sqrt(hp)))
datatable.4 <- datatable %>% mutate(Historical.VaR = abs(quantile(mean*100,1-a)*Last*az))

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
comparables <- comparables[,-c(45)]

#View(datatable.4)

comparables <- comparables[!duplicated(comparables$ticker),]
datatable.4 <- datatable.4[!duplicated(datatable.4$ticker),]

comparables$Sector <- as.factor(comparables$Sector)
l <- levels(comparables$Sector)
l

FLATFILE <- comparables[with(comparables, grepl("Mining",Sector )),]




#companies <- companies[-2]
#dataTable <- as.data.frame(inner_join(beta.table, metric_conversion, by = "ticker"))

#data_output/stock_stats_MX-Sim_10.04.2016.csv
#trendPlots/Other.tickers/US_Undervalued.09.26.2016_data.csvTSX_2017.10.20.csv
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(comparables,"data_output/stock_stats_TSX_FAMA5_2017.10.20.csv")
