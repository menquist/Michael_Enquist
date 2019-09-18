#######################################################################################################
##########################################################################################################
# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

# Plug ticker in
#-----------------------------------------
ticker <- " AD.TO" # <== plug in here
#-----------------------------------------


library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------

#FRED/CSHCCPCAA156NRUG

CAD <- Quandl("CUR/CAD", start_date=year, collapse="daily")
#GOLD <- Quandl("LBMA/GOLD", start_date=year, collapse="daily")
### Cost to register property (% of property value) - Hong Kong SAR, China
#OPEC <- Quandl("OPEC/ORB", start_date=year, collapse="daily")
ggplot(OPEC, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
#ggplot(GOLD, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
#colnames(Percent.of.property.value.HK)[2] <- c("Cost to register property (% of property value)") 
CAD.USA.EX <- Quandl("BOE/XUMLCDD",collapse="daily", start_date=year)
#CAD.US <- getSymbols(symbols, src="FRED", auto.assign = FALSE)
CAD.USA.EX <- getSymbols("CADUSD=X", src = "yahoo", from = year, 
                         auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]



setwd("~/Downloads")
symbols <- read.table("Norway.csv", quote="", stringsAsFactors=FALSE)$V1
#symbols <- c("TSLA", "NFLX", "GOOG", "APPL","DIS")
# Read in ticker symbols by using the NASDAQ.csv file

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
  tryit <- try(getSymbols(symbol, src="FRED"))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from=year, src="FRED")
    dataset <- merge(dataset, (get(symbols[i])))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}


str(dataset)

#dat  <- data.frame(date=index(dataset), coredata(dataset))
#index(dataset) <- round(index(dataset),"day")


rm(list=setdiff(ls(), c("dataset", "perform_vec","symbols","year")))



dat1 <- as.data.frame(dataset)
dat1 <- na.omit(dat1)
nr <- nrow(dat1)
dat1 <- (dat1[-1,] - dat1[-nr,]) / dat1[-nr,]



library(functional)

do.call(data.frame,lapply(dat1, function(x) replace(x, is.infinite(x),NA)))


test <- sapply(dat1, function(col) ifelse(col>0,1,ifelse(col<=0,0,col)))
test <- data.frame(test)



for(i in 1:length(stockname)) {
  if(exists("dat")) {
    Stock_df <- data.frame( date = date, rate = test[[i]])
    write.csv(Stock_df, paste("~/R_Stuff/Stock_Analysis/Stock.out18", stockname[i],sep = "/"))
  } else {
    print(paste0("missing #",i))
  }
}



data <- data.frame(dat1)
data <- cbind(ticker = rownames(data), data)
HKSE <- data[-1] 
# Logistic Indicator
data_logit <- HKSE %>%
  mutate(long = ifelse(IRLTLT01NOM156N < 0, 0, 1)) %>%
  mutate(short = ifelse(IRLTLT01NOM156N > 0, 0, 1))
data_logit <- na.omit(data_logit)

str(data_logit)
data_logit
rate <- data_logit[,4]
data_logit <- data_logit[,-c(1,4,5)]
# Predict long
#testlogit <- data_logit[,2:3]
#longlegit <- data_logit[,4]
#logitmerge <- cbind(testlogit,longlegit)
#na.action(na.omit), na.action(na.omit)
logit_long <- glm(rate ~ .,na.action(na.omit), data = data_logit, family = "binomial")
x <- logit_long$coefficients[1]
x <- logit_long$coefficients[3]

summary(logit_long)
str(logit_long)


len<- nrow(data_logit)

fit = glm(rate ~ rate.OPEC, data=data_logit, family=binomial)
summary(fit)
newdat <- data.frame(hp=seq(min(rate), max(data_logit$rate.OPEC),len=len))
newdat$vs = predict(fit, newdata=data_logit, type="response")
plot(rate~rate.ZEO, data=data_logit, col="red4")
lines(rate ~ rate.ZEO, data_logit, col="green4", lwd=2)

data_logit <- na.omit(data_logit)

#ggplot(data_logit, aes(rate, as.numeric(rate.ZEO)-1, color=long)) +
#  stat_smooth(method="loess", formula=rate ~ rate.ZEO,
#              alpha=0.2, size=2, aes(fill=long)) +
#  geom_point(position=position_jitter(height=0.03, width=0)) +
#  xlab("Age") + ylab("Pr (survived)")

library(glmnet)

model<-glmnet(as.matrix(data_logit), as.integer(rate), family="binomial")
coef<-predict(model, type="coefficients")
lasso <- cv.glmnet(as.matrix(data_logit),as.integer(rate), family="binomial") 
lasso_coef<-predict(lasso, type="coefficients")
plot(model, label = TRUE)
plot(lasso)
plot(coef)
plot(lasso_coef)

str(logitmerge)
library(ResourceSelection)
hoslem.test(as.numeric(HKSE$rate.HKSE), fitted(mod))




hist(HKSE$`0012.HK.csv`)



dat  <- data.frame(date=round(index(dataset[-1,],"day")), coredata(dat1))
dat <- cbind(ticker = rownames(dat), dat)
rownames(dat) <- 1:nrow(dat)
date  <- dat[,2]
dat <- dat[,-c(1,2)]
date <- round(date,"days")
stockname <- colnames(test)

#stockname <-gsub("X", "", stockname)
#data <- dat[100:200]

stockname <-gsub(".Adjusted", "", stockname)




data <- gsub('[.]', '-', stockname)
stockname <- gsub('\\-TO*', '.TO', data)








what_metrics <- yahooQF(perform_vec)
metrics <- getQuote(stockname, what=what_metrics)
metrics <- cbind(ticker = rownames(metrics), metrics)
rownames(metrics) <- 1:nrow(metrics)

#str(metrics)



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
fromDate <- year

oil.NYSE <- getSymbols("^GSPTSE", src = "yahoo", from = fromDate, 
                       auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]



# get Index returns
NYSE.returns <- data.frame(date = as.factor(time(oil.NYSE)), price = as.numeric(oil.NYSE))  %>%
  mutate(rate.NYSE = (price - lag(price))/lag(price)) %>%
  filter(date != year)  %>% 
  select(-price)

#NYSE.returns <- NYSE.returns[-1,]


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
  
  
  mod <- glm(rate ~ rate.NYSE, data = joined.ret, family = "binomial")
  intercept <- mod$coefficients[1]
  beta <- mod$coefficients[2]
  
  
  # regress returns
  #mod <- lm(rate ~ rate.NYSE, na.action=na.omit, data = joined.ret)
  #intercept <- mod$coef[1]
  #beta <- mod$coef[2]
  
  # return int, beta and data
  return(list(model = mod, int = intercept, beta = beta, 
              rate = joined.ret[,3], date = joined.ret$date))
}

# output table of stock stats
beta.df <- data.frame(ticker = list.files("Stock.out18/"),alpha = ldply(capm[,2])[,2], beta = ldply(capm[,3])[,2])
beta.table <- as.data.frame(inner_join(Volatility_Table, beta.df, by = 'ticker'))

beta.table <- beta.table %>% 
  group_by(ticker) %>% 
  mutate(CAPM = .0189 + alpha + (beta*(mean - .0189))) 
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
companies <- inner_join(beta.table, metric_conversion, by= "ticker")
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
comparables <- comparables[,-c(40)]

#View(datatable.4)

comparables <- comparables[!duplicated(comparables$ticker),]
datatable.4 <- datatable.4[!duplicated(datatable.4$ticker),]





#companies <- companies[-2]
#dataTable <- as.data.frame(inner_join(beta.table, metric_conversion, by = "ticker"))

#data_output/stock_stats_MX-Sim_10.04.2016.csv
#trendPlots/Other.tickers/US_Undervalued.09.26.2016_data.csv
#__________________________________________________________________
# write table to ALTERNATE working directory
write.csv(comparables,"data_output/stock_stats_MX-Sim_2017.10.24-log.csv")
#__________________________________________________________________


library(beepr)
beep(4)





