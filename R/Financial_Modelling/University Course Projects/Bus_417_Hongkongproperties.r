library(tidyr)

s <- c("1113.HK","0086.HK",".0012.HK")

fin <- lapply(s, getFinancials, auto.assign=T)

year <- "1900-01-01"


setwd("~/R_Stuff/Stock_Analysis/Stock.out15/")
SP <-  getSymbols(Symbols = "RRX.TO", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
#SP <-data.frame(date=as.character(index(SP)),coredata(SP))
#SP.adj <- SP %>% 
#  mutate(HSKE = 100*((SP[2] - lag(SP[2])))/SP[2])
#SP.adj <- na.omit(SP.adj[-2])
RRX.TO <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.HKSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)




temp = list.files(pattern="RRX") 
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names =T )
  datalist = try(lapply(temp,function(x){read.csv(file=x,header=T,colClasses=c("NULL", NA, NA))}))
  try(Reduce(function(x,y) {merge(x, y,  all.x=TRUE)}, datalist))
}
#by="date",

mydata <- multmerge("")
str(mydata)
#col.names = colnames(read.table(file ="headers_28_subset.txt", header = T))
col.names = temp

names(mydata)[2:1500]=col.names
#mydata[-4]
#str(mydata)
#mydata.1 <- mydata %>% 
#  filter( ticker == "0012.HK.csv" )


boxplot(, mydata[,2])
basicStats(na.omit(mydata))
boxplot(mydata[,2])

alvR <- na.omit(mydata$rate)
hist(alvR,probability=T,xlab="ALV.DE returns",main=NULL)



alv <- na.omit(mydata$rate); 
DS <- density(alv)
yl=c(min(DS$y),max(DS$y)) #set y limits
hist(alv,probability=T,xlab="ALV returns", main=NULL,ylim=yl)
rug(alv); lines(DS); a=seq(min(alv),max(alv),0.001)
points(a,dnorm(a,mean(alv),sd(alv)), type="l",lty=2)
 # if you rather have a red line for the normal distribution do:
lines(a,dnorm(a,mean(alv), sd(alv)),col="red")

shapiro.test(alv)


#valid_column_names <- make.names(names=names(mydata), unique=TRUE, allow_ = TRUE)
#names(mydata) <- valid_column_names
#### choose tickers!!!!
#date <- mydata$date
#mydata.1  <- mydata
#mydata <- cbind(date,mydata.1)
#######################
combined <- sort(union(levels(NYSE.returns$date), levels(NYSE.returns$date)))
n <- left_join(mutate(NYSE.returns, a=factor(date, levels=combined)),
               mutate(mydata, a=factor(date, levels=combined)))
n <- n[-3]

n <- na.omit(n)
str(n)

HSKE <- n  
HKSE <- HSKE[-1]  
mod <- lm(rate.HKSE~., data = HKSE)
summary(mod)
 
HKSE <- data[-1] 
# Logistic Indicator
data_logit <- HKSE %>%
  mutate(long = ifelse(rate.NYSE < 0, 0, 1)) %>%
  mutate(short = ifelse(rate.NYSE > 0, 0, 1))
data_logit <- na.omit(data_logit)

str(data_logit)
data_logit
rate <- data_logit[,7]
data_logit <- data_logit[,-c(2,7,8)]
# Predict long
testlogit <- data_logit[,2:3]
longlegit <- data_logit[,4]
logitmerge <- cbind(testlogit,longlegit)

logit_long <- glm(rate ~ ., data = data_logit, family = "binomial")
x<- logit_long$coefficients[1,]


summary(logit_long)
str(logit_long)

data_logit <- na.omit(data_logit)

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





#mydata.1 <- na.omit(n)
setwd("~/R_Stuff/Stock_Analysis/data_output")
CK.Properties <-   list.files(pattern="*HKSE")
myfiles = lapply(CK.Properties,read.delim)
str(myfiles)



temp = list.files(pattern = "*HKSE")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


temp = list.files(pattern="*HKSE")
list2env(
  lapply(setNames(temp, make.names(gsub("*HKSE$", "", temp))), 
         read.csv), envir = .GlobalEnv)
str(stock_stats_HKSE_April_26_2016.csv)

#distinct_df = temp[13] %>% distinct(ticker) %>% select("1113.HK")


HKSE <- rbind(stock_stats_HKSE_2016.01.16.csv ,
              stock_stats_HKSE_2016.02.16.csv, stock_stats_HKSE_2016.03.04.csv,stock_stats_HKSE_2016.04.26.csv,stock_stats_HKSE_2016.06.10.csv)
HKSE.1 <- rbind(stock_stats_HKSE_2016.07.08.csv,stock_stats_HKSE_2016.08.05.csv,stock_stats_HKSE_2016.09.05.csv,
              stock_stats_HKSE_2016.10.01.csv,stock_stats_HKSE_2016.12.17.csv,stock_stats_HKSE_2017.01.13.csv,stock_stats_HKSE_2017.02.03.csv,stock_stats_HKSE_2017.03.06.csv)

NYSE.3 <- HKSE.1 %>% 
  select(X,ticker,mean,sd,beta,Earnings.Share,Market.Capitalization,EBITDA,P.E.Ratio,
         Volume,X52.week.Low,X52.week.High,Last,Name)


combine <- rbind(HKSE,NYSE.3)

ck.2 <- combine %>% 
  filter( ticker == "0012.HK")

ck.3 <- HKSE.1 %>% 
  filter( ticker == "0012.HK")

setwd("~/R_Stuff/Stock_Analysis")
write.csv(ck.3,"Corporate Financials/Henderson_Land_Development_Semi-Trend.Analysis_2017.03.14.csv" )



#ck <- HKSE %>% 
#  filter( ticker == "1113.HK")
#ck.1 <- HKSE.1 %>% 
#  filter( ticker == "1113.HK")

#newData <- as.data.frame(left_join(HKSE, NYSE.3, by = "ticker")) %>%
#  mutate(ticker = as.factor(ticker))
#str(newData)

#DF_obj <- lapply(ls(), get)
#str(DF_obj)

#DF_obj <- data.frame(DF_obj)

#df <- data.frame(matrix(unlist(DF_obj), nrow=T, byrow=T))
#do.call(rbind, lapply(DF_obj, data.frame, stringsAsFactors=FALSE))
#str(DF)

#DF <- join_all(list(DF_obj,stock_stats_HKSE_2017.03.11.csv ), by='ticker', type='left')

#big.list.of.data.frames <- lapply(HKSE, read.table,header = TRUE,
#                                  stringsAsFactors = FALSE)



#newData <- as.data.frame(inner_join(DF_obj, stock_stats_HKSE_2017.03.11.csv, by = "ticker")) %>%
#  mutate(ticker = as.factor(ticker))

#DF <- do.call("rbind", list(DF_obj))
#str(DF)
#df1 <- data.frame(Intercept = .4, x1=.4, x2=.2, x3=.7)
#df2 <- data.frame(Interceptlego = .5,        x2=.8       )
#myList <- list(df1, df2)
#dat <- data.frame()
#for(i in seq(along=myList)) for(j in names(myList[[i]]))
#  dat[i,j] <- myList[[i]][j]
#dat
#l <- DF_obj
#do.call(rbind, lapply(lapply(l, unlist), "[",
#                      unique(unlist(c(sapply(l,names))))))
#c(sapply(l,names))
#unique(unlist(c(sapply(l,names))))
#lapply(l, unlist) 
#listOfVectors <- lapply(lapply(l, unlist), "[",
#                        unique(unlist(c(sapply(l,names)))))
#do.call(rbind, listOfVectors)
#DF <- do.call(rbind, lapply(lapply(l, unlist), "[",
#                            unique(unlist(c(sapply(l,names))))))
#str(DF)


#datalist <- mget(ls(pattern = "stock_stats_HKSE_2016.0[1-4]"))

#DF_obj %>% group_by(ticker) %>%
#  filter(ticker == "1113.HK")

#rbind.fill(Dat1, Dat2)
#ck <- DF %>% 
#  filter( ticker == "1113.HK")


#ls()

#dfs = sapply(.GlobalEnv, is.data.frame) 
#dfs

#rm(temp,CK.Properties,myfiles,ck)

#dfs = sapply(ls(), is.data.frame) 
#lapply(mget(names(dfs)[dfs]), "ck", drop = FALSE)
#do.call(rbind, mget(names(dfs)[dfs]))
library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")

### Cost to register property (% of property value) - Hong Kong SAR, China
Percent.of.property.value.HK <- Quandl("WDBU/HKG_IC_RP_COST", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(Percent.of.property.value.HK, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(Percent.of.property.value.HK)[2] <- c("Cost to register property (% of property value)") 

### Cost to register property (% of property value) - - Canada
Percent.of.property.value.CAN <- Quandl("WDBU/CAN_IC_RP_COST", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")

###Commercial Property Prices, Exist.Retail Premises, Per Sq.M., M-All Nsa: Hong Kong SAR
HK.Commercial.Property.Prices <- Quandl("BIS/RE_MHK0C0110", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(HK.Commercial.Property.Prices, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(HK.Commercial.Property.Prices)[2] <- c("Commercial Property Prices, Exist.Retail Premises, Per Sq.M") 


###Commercial Property Prices, Exist.Retail Premises, Per Sq.M., M-All Nsa: Hong Kong SAR
#HK.Commercial.Property.Prices <- Quandl("BIS/RE_MHK0C0110", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")


###Residential Property Prices, Exist.Retail Premises, Per Sq.M., M-All Nsa: Hong Kong SAR
HK.Residential.Property.Prices <- Quandl("BIS/RE_MHK010110", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(HK.Residential.Property.Prices, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(HK.Residential.Property.Prices)[2] <- c("Residential Property Prices, Exist.Retail Premises, Per Sq.M") 



###Real interest rate (%) - Hong Kong SAR, China
HK.Real.Interest.Rate <- Quandl("WWDI/HKG_FR_INR_RINR", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(HK.Real.Interest.Rate, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(HK.Real.Interest.Rate)[2] <- c("Real interest rate (%) - Hong Kong SAR, China") 



###Deposit interest rate (%) - Hong Kong SAR, China
HK.Deposit.Interest.Rate <- Quandl("WWDI/HKG_FR_INR_DPST", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
colnames(HK.Deposit.Interest.Rate)[2] <- c("Deposit interest rate (%) - Hong Kong SAR, China") 



###Lending interest rate (%) - Hong Kong SAR, China
HK.Lending.Interest.Rate <- Quandl("WWDI/HKG_FR_INR_LEND", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(HK.Lending.Interest.Rate, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "red") + theme_bw()
colnames(HK.Lending.Interest.Rate)[2] <- c("Lending interest rate (%) - Hong Kong SAR, China") 



###HONG KONG -- SPOT EXCHANGE RATE, HK$/US$, Monthly
HK.US.Exchange.Rate <- Quandl("FED/RXI_N_M_HK", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
ggplot(HK.US.Exchange.Rate, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "red") + theme_bw()
colnames(HK.US.Exchange.Rate)[2] <- c("HONG KONG -- SPOT EXCHANGE RATE, HK$/US$") 


#Hong.Kong.Data <- cbind(Percent.of.property.value.HK,Percent.of.property.value.CAN,HK.Commercial.Property.Prices,HK.Residential.Property.Prices,HK.Real.Interest.Rate,HK.Deposit.Interest.Rate,HK.Lending.Interest.Rate,HK.US.Exchange.Rate)

L <- list(Percent.of.property.value.HK,HK.Commercial.Property.Prices,HK.Residential.Property.Prices,HK.Real.Interest.Rate,HK.Deposit.Interest.Rate,HK.Lending.Interest.Rate,HK.US.Exchange.Rate,HK.Population)

 <- Reduce(inner_join,L)
write.csv(Hong.Kong.Data,"Hong.Kong.Data.csv")


###HONG KONG Population - Hong Kong, China
HK.Population <- Quandl("ADB/POP_HKG", start_date="2000-01-01", end_date="2017-03-01", collapse="annual")
HK.Population <- HK.Population[-5] 
ggplot(HK.Population, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "red") + theme_bw()
colnames(HK.Population)[2] <- c("HONG KONG -- SPOT EXCHANGE RATE, HK$/US$") 



Coffee_Market <- ggplot(HK.Lending.Interest.Rate, aes(x =Date)) + 
  geom_line(aes(y = Value, colour = "Brazilian Real")) + 
  theme(plot.title = element_text(size=15, face="bold", hjust=2)) + theme_bw() +
  scale_fill_continuous(name="Percentage.Change")  + ylab("Percentage.Change") + xlab("Years 1996 to 2015")
Coffee_Market 



FRED/AEXHKUS

Brazilexch <- Quandl("BNP/USDBRL", start_date="2000-01-01", end_date="2015-12-31", collapse="monthly")

### Brazil Commodities and GDP


Brazil_Comm <- Quandl(c("ODA/PCOFFOTM_USD"), start_date="2000-01-01", end_date="2015-12-31",
                      collapse="monthly")



#####################################################################################################################


setwd("~/Downloads/HLD/")
BS.2 <- read.csv("HLD BS HKD.csv", header = T ,skip =2) # skip= 4,header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
str(BS.2)


CF.2 <- read.csv("HLD CF HKD.csv",header = T, skip = 3)
str(CF.2)
IS.2 <- read.csv("HLD IS HKD.csv", skip = 3)
str(IS.2)


BS.1 <- melt(BS.2, id=c("In.Millions.of.HKD.except.Per.Share"))
CF.1 <- melt(CF.2, id=c("In.Millions.of.HKD.except.Per.Share"))
IS.1 <- melt(IS.2, id=c("In.Millions.of.HKD.except.Per.Share"))
str(IS.1)
##### VAR1 ANd VAr2
names(BS.1) <- c("X1", "X2", "value")
names(CF.1) <- c("X1", "X2", "value")  
names(IS.1) <- c("X1", "X2", "value")

str(BS.1)

###Total Assets
a3 <- filter(BS.1, X1 == "Total Assets")# %>% 
  

a3 <- a3[-c(1:4),]
str(a3)
toBeRemoved<-which(a3$value=="")
a3<-a3[-toBeRemoved,]
a3$value <- as.numeric(gsub(",","", a3$value))
str(a3)
names(a3) <- c("X1","date", "value")
a3

#### Total Equity
a4 <- filter(BS.1, X1 == "Total Equity") 
a4 <- a4[-c(1),]
a4$value <- as.numeric(gsub(",","", a4$value))
str(a4)
names(a4) <- c("X1","date", "value")
ggplot(a4, aes(y = value, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()


#### Total DEBT
#grep(LT Debt)
#myfun <- function(x,y) names(y[grep(x, names(y))])
#a5 <-  myfun("LT Debt", BS.1)
#a5 <- a5 %>% 
#  select(X1,contains("Debt"))
keywords <- BS.1$X1[grep("*LT Debt", BS.1$X1)]
keywords.1 <- BS.1$X1[grep("*ST Debt", BS.1$X1)]

a5 <- BS.1 %>% 
  filter(X1 %in% keywords)
a5.1 <- BS.1 %>% 
  filter(X1 %in% keywords.1)
a5 <- as.data.frame(inner_join(a5,a5.1, by = 'X2'))

a5 <- a5[-c(1),]
str(a5)

a5$value.x <- as.numeric(gsub(",","", a5$value.x))
a5$value.y <- as.numeric(gsub(",","", a5$value.y))
a5 <-  mutate(a5,Total.Debt = value.x + value.y)
#toBeRemoved<-which(a4$value=="")
#a4<-a4[-toBeRemoved,]
a5 <- a5 %>% 
  select(X2,Total.Debt)
names(a5) <- c("date", "value")
ggplot(a5, aes(y = value, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()

#### Total Liabilites

a6 <- filter(BS.1, X1 == "Total Liabilities") 

a6 <- a6[-c(1),]
str(a6)
a6 <- a6[-1]
#toBeRemoved<-which(a4$value=="")
#a4<-a4[-toBeRemoved,]
a6$value <- as.numeric(gsub(",","", a6$value))
str(a6)
names(a6) <- c("date", "value")
ggplot(a6, aes(y = value, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()

#### for the altman Total Liabilities
Total_Liabilities  <- a6$value


#### Total Revenue 
Total.Revenue <- filter(IS.1, X1 == "Revenue") 
Total.Revenue <- Total.Revenue[-c(1),]
str(Total.Revenue)
Total.Revenue <- Total.Revenue[-1]
#toBeRemoved<-which(a4$value=="")
#a4<-a4[-toBeRemoved,]
Total.Revenue$value <- as.numeric(gsub(",","", Total.Revenue$value))
str(Total.Revenue)
names(Total.Revenue) <- c("date", "value")
ggplot(Total.Revenue, aes(y = value, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()

#################### Debt Equity

DEBTEQUITY <- filter(BS.1, X1 %in% c("Total Equity")) %>%
  dcast(X2 ~ X1)

DEBTEQUITY <- DEBTEQUITY[-c(1),]
names(DEBTEQUITY) <- c("date", "value")
DEBTEQUITY$value <- as.numeric(gsub(",","", DEBTEQUITY$value))


str(DEBTEQUITY)
DEBTEQUITY <- as.data.frame(inner_join(DEBTEQUITY, a5, by="date"))

names(DEBTEQUITY) <- c("date", "equity", "debt")
DEBTEQUITY <- mutate(DEBTEQUITY, DE = debt/equity) 
ggplot(DEBTEQUITY, aes(y = DE, x = date)) + geom_point(size = 3, colour = "blue") + theme_bw()

###### Create Rate of Change Function
ROC.Function <- function(z,x,y){
  Matrix <- cbind(y,x,0)
  colnames(Matrix) <- c("date","X1", "value")
  ROC <- NULL 
  for(i in 1:nrow(z)){ ROC[i] <- (100*(x[1+i] - x[i])/-x[i])
  Matrix[i,3] <- ROC[i]
  
  }
  return(Matrix)
}

###### Create Rate of Change Function
ROC.Function.Small.Ratios <- function(z,x,y){
  Matrix <- cbind(y,x,0)
  colnames(Matrix) <- c("date","X1", "value")
  ROC <- NULL 
  for(i in 1:nrow(z)){ ROC[i] <- (100*((x[1+i] - x[i])/x[i]))
  Matrix[i,3] <- ROC[i]
  
  }
  return(Matrix)
  }
  
Matrix.Total.Assets <- as.data.frame(ROC.Function(a3,a3$value,a3$date))
K <- Matrix.Total.Assets
#col_idx <- grep("NA", names(K))
#Matrix.Total.Assets <- K[, c(col_idx, (1:ncol(K))[-col_idx])]
#Matrix.Total.Assets <- na.omit(transform(K, value = c(NA, value[-nrow(K)])))
Matrix.Total.Assets <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Total.Assets)[2:3] <- c("Total Assets", "%Change.of.Total.Assets") 
str(Matrix.Total.Assets)

Matrix.Total.Equity <- as.data.frame(ROC.Function(a4,a4$value,a4$date))
K <- Matrix.Total.Equity
#col_idx <- grep("NA", names(K))
#Matrix.Total.Assets <- K[, c(col_idx, (1:ncol(K))[-col_idx])]
Matrix.Total.Equity <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Total.Equity)[2:3] <- c("Total Equity", "%Change.of.Total.Equity") 
str(Matrix.Total.Equity)

Matrix.Total.Debt <- as.data.frame(ROC.Function(a5,a5$value,a5$date))
K <- Matrix.Total.Debt
#col_idx <- grep("NA", names(K))
#Matrix.Total.Assets <- K[, c(col_idx, (1:ncol(K))[-col_idx])]
Matrix.Total.Debt <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Total.Debt)[2:3] <- c("Total Debt", "%Change.of.Total.Debt") 
str(Matrix.Total.Debt)

Matrix.Total.Liabilities <- as.data.frame(ROC.Function(a6,a6$value,a6$date))
K <- Matrix.Total.Liabilities
#col_idx <- grep("NA", names(K))
#Matrix.Total.Assets <- K[, c(col_idx, (1:ncol(K))[-col_idx])]
Matrix.Total.Liabilities <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Total.Liabilities)[2:3] <- c("Total Liabilities", "%Change.of.Total.Liabilities") 
str(Matrix.Total.Liabilities)

Matrix.DEBTEQUITY <- ROC.Function(DEBTEQUITY,DEBTEQUITY$DE,DEBTEQUITY$date) 
Matrix.DEBTEQUITY <- transform(Matrix.DEBTEQUITY, value = c(NA, value[-nrow(Matrix.DEBTEQUITY)]))
colnames(Matrix.DEBTEQUITY)[2:3] <- c("Debt.Equity.Ratio","%Change.of.Debt.Equity") 
str(Matrix.DEBTEQUITY)

Matrix.Total.Revenue <- ROC.Function(Total.Revenue,Total.Revenue$value,Total.Revenue$date) 
K <- Matrix.Total.Revenue
Matrix.Total.Revenue <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Total.Revenue)[2:3] <- c("Total.Revenue","%Change.of.Total.Revenue") 
str(Matrix.Total.Revenue)

L <- list(Matrix.Total.Assets, Matrix.Total.Equity, Matrix.Total.Debt,Matrix.Total.Liabilities,Matrix.DEBTEQUITY,Matrix.Total.Revenue)
Rate.of.Change <- Reduce(inner_join, L)

###############################################################ENTERPRISE VALUE


#qq
#qqq <- qq$Last
#qqq
####
#string <- c("+ Minority/Non Controlling Interest", "+ Cash, Cash Equivalents & STI" ,  "+ Preferred Equity"
#            ,"+ Common Stock")
#keywords <- BS.1$X1 %in% string #  grepl(string, BS.1$X1, fixed = TRUE)

keywords <- BS.1$X1[grep("+ Minority/Non Controlling Interest|+ Cash, Cash Equivalents & STI|+ Preferred Equity|Shares Outstanding", BS.1$X1)]


enterprise <- filter(BS.1, X1 %in%  keywords) 
#colnames(enterprise) <- c( "date", "cash", "minority","stock", "total", "debt" )

enterprise <- enterprise[-c(1:4),]
str(enterprise)
enterprise$value <- as.numeric(gsub(",","", enterprise$value))
enterprise <- na.omit(enterprise)
enterprise <- enterprise %>% spread(X1, value)

enterprise[is.na(enterprise)] <- 0
colnames(enterprise) <- c( "date", "cash", "minority","stock", "total" )
enterprise <- inner_join(enterprise,a5,by="date")
colnames(enterprise) <- c( "date","cash", "minority","stock", "total", "debt" )
qqq <- 46.20

ev.fun <- function(x) {
  if(sum(is.na(enterprise$minority)) == 0 & sum(is.na(enterprise$stock)) == 0) {
    Enterprise.Value <- mutate(x, Enterprise.Value = (total*qqq)+debt+minority+stock-cash)
  } else {
    if(sum(is.na(enterprise$minority)) == 0 & sum(is.na(enterprise$stock)) > 0) {
      Enterprise.Value <- mutate(x, Enterprise.Value = (total*qqq)+debt-cash) 
    } else{
      if(sum(is.na(enterprise$minority)) > 0 & sum(is.na(enterprise$stock)) == 0) {
        Enterprise.Value <- mutate(x, Enterprise.Value = (total*qqq)+debt+stock-cash)
      } else {
        Enterprise.Value <- mutate(x, Enterprise.Value = (total*qqq)+debt-cash)
      }
    }
  }
  
  return(Enterprise.Value)
}

enterprise_out <- ev.fun(enterprise)
enterprise_out

Matrix.Enterprise.Value <- ROC.Function(enterprise_out,enterprise_out$Enterprise.Value,enterprise_out$date) 
K <- Matrix.Enterprise.Value
Matrix.Enterprise.Value <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Enterprise.Value)[2:3] <- c("Enterprise.Value","%Change.of.Enterprise.Value") 
str(Matrix.Enterprise.Value)
Matrix.Enterprise.Value <- as.data.frame(Matrix.Enterprise.Value)


############################################################################ EBITDA

keywords <- IS.1$X1[grep("EBITDA|EBITDA Margin (T12M)|Shares Outstanding|^EBIT$|Profit Margin|Net Income, GAAP|^Revenue$", IS.1$X1)]
EBITDA <- filter(IS.1, X1 %in%  keywords) 

EBITDA <- EBITDA[-c(1:6),]
str(EBITDA)
EBITDA$value <- as.numeric(gsub(",","", EBITDA$value))
EBITDA <- na.omit(EBITDA)
EBITDA <- EBITDA %>% spread(X1, value)
colnames(EBITDA)[1]<- c("date")

#####################################################################Short term debt riskiness
keywords <- BS.1$X1[grep("LT Debt", BS.1$X1)]
ltdebt <- filter(BS.1, X1 %in% keywords) 
ltdebt <- ltdebt[-c(1),]
str(ltdebt)
ltdebt$value <- as.numeric(gsub(",","", ltdebt$value))
ltdebt <- ltdebt[-1]
colnames(ltdebt)<- c("date","LT.Debt") 
ltdebt <- inner_join(ltdebt,a5,by="date")
colnames(ltdebt) <- c("date", "ldebt", "totdebt")
ltd_ratio <- mutate(ltdebt, Short.term.debt.riskiness = ldebt/totdebt)
ltd_ratio
Matrix.Short.term.debt.riskiness <- ROC.Function(ltd_ratio,ltd_ratio$Short.term.debt.riskiness,ltd_ratio$date) 
K <- Matrix.Short.term.debt.riskiness
Matrix.Short.term.debt.riskiness <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Short.term.debt.riskiness)[2:3] <- c("Short.term.debt.riskiness","%Change.of.Short.term.debt.riskiness") 
str(Matrix.Short.term.debt.riskiness)

######################################################################EBIT MArgin
colnames(EBITDA)<- c("date","EBIT", "EBITDA","EBITDA.Margin", "Net.Income", "Profit.Margin", "Revenue" )
EBITDA.1 <- mutate(EBITDA, EBIT.Margin =  EBIT/Revenue,
                   Net.Income.Margin = Net.Income/Revenue)

ebitM_rates  <- 100*(EBIT_Margin$EbitM[2] - EBIT_Margin$EbitM[1])/EBIT_Margin$EbitM[1]
ebitM_rates <- NULL
for(i in 1:nrow(EBIT_Margin)) ebitM_rates[i] <- 100*(EBIT_Margin$EbitM[1+i] - EBIT_Margin$EbitM[i])/EBIT_Margin$EbitM[i]
ebitM_rates

Matrix.EBIT.Margin <- ROC.Function(EBITDA.1,EBITDA.1$EBIT.Margin,EBITDA.1$date) 
K <- Matrix.EBIT.Margin
Matrix.EBIT.Margin <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.EBIT.Margin)[2:3] <- c("EBIT.Margin","%Change.of.EBIT.Margin") 
str(Matrix.EBIT.Margin)

Matrix.Net.Income.Margin <- ROC.Function(EBITDA.1,EBITDA.1$Net.Income.Margin,EBITDA.1$date) 
K <- Matrix.Net.Income.Margin
Matrix.Net.Income.Margin <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Net.Income.Margin)[2:3] <- c("Net.Income.Margin","%Change.of.Net.Income.Margin") 
str(Matrix.Net.Income.Margin)

########################################################################### ROIC 

### average net debt
keywords <- BS.1$X1[grep("^Net Debt$", BS.1$X1)]
Net.Debt <- filter(BS.1, X1 %in% keywords) 
Net.Debt <- Net.Debt[-c(1),]
str(Net.Debt)
Net.Debt$value <- as.numeric(gsub(",","", Net.Debt$value))
Net.Debt <- Net.Debt[-1]
colnames(Net.Debt)<- c("date","Net.Debt") 
Net.Debt <- inner_join(Net.Debt,a4,by="date")
ebit <-  EBITDA[1:2]
Net.Debt.1 <- inner_join(Net.Debt,ebit,by="date")

## ROIC
ROIC <- mutate(Net.Debt.1, ROIC.1 = EBIT/(Net.Debt+value))

Matrix.ROIC <- ROC.Function(ROIC,ROIC$ROIC.1,ROIC$date) 
K <- Matrix.ROIC
Matrix.ROIC <- as.data.frame(Matrix.ROIC)
#Matrix.ROIC <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.ROIC)[2:3] <- c("ROIC","%Change.of.ROIC") 
str(Matrix.ROIC)


##################################################################### ROE
NI <- filter(IS.1, X1 == "Net Income, GAAP") 
NI <- NI[-1]
colnames(NI) <- c("date", "NI")


ROE <- inner_join(NI,a4, by="date")
ROE$NI <- as.numeric(gsub(",","", ROE$NI))
ROE <- mutate(ROE, ROE = NI/value)
Matrix.ROE <- ROC.Function(ROE,ROE$ROE,ROE$date) 
K <- Matrix.ROE
Matrix.ROE <- as.data.frame(Matrix.ROE)
#Matrix.ROE <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.ROE)[2:3] <- c("ROE","%Change.of.ROE") 
str(Matrix.ROE)

############### Net Cash
net.cash  <- filter(CF.1, X1 == "Net Changes in Cash")[-1]
colnames(net.cash) <- c("date", "Net.Changes.in.Cash")
net.cash <- net.cash[-c(1:3),]
net.cash$Net.Changes.in.Cash <- as.numeric(gsub(",","", net.cash$Net.Changes.in.Cash))

Matrix.Net.Changes.in.Cash <- ROC.Function(net.cash,net.cash$Net.Changes.in.Cash,net.cash$date) 
K <- Matrix.Net.Changes.in.Cash
#Matrix.Net.Changes.in.Cash <- as.data.frame(Matrix.ROE)
Matrix.Net.Changes.in.Cash <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Net.Changes.in.Cash)[2:3] <- c("Net.Changes.in.Cash","%Change.of.Net.Changes.in.Cash") 
str(Matrix.Net.Changes.in.Cash)

#################### account payable
keywords <- BS.1$X1[grep("+ Accounts Payable", BS.1$X1)]
acccounts.payable <- filter(BS.1, X1 %in% keywords)[-1] 
colnames(acccounts.payable) <- c("date", "acccounts.payable")
acccounts.payable <- acccounts.payable[-c(1),]
acccounts.payable$acccounts.payable <- as.numeric(gsub(",","", acccounts.payable$acccounts.payable))


###################################################################### Shareholders Equity

Shareholders_Equity <- filter(BS.1, X1 %in% c("Total Assets", "Total Liabilities"))
Shareholders_Equity <- Shareholders_Equity[-c(1:3),]
str(Shareholders_Equity)
Shareholders_Equity$value <- as.numeric(gsub(",","", Shareholders_Equity$value))
Shareholders_Equity <- na.omit(Shareholders_Equity)
Shareholders_Equity <- Shareholders_Equity %>% spread(X1, value)
colnames(Shareholders_Equity)<- c("date","Total.Assets" , "Total.Liabilities")
ShareholdersEquity <- mutate(Shareholders_Equity, Shareholders.Equity = Total.Assets-Total.Liabilities)


##################################################################### Working capital

Working_capital <- filter(BS.1, X1 %in% c("Total Current Assets", "Total Current Liabilities"))
Working_capital <- Working_capital[-c(1:2),]
str(Working_capital)
Working_capital$value <- as.numeric(gsub(",","", Working_capital$value))
Working_capital <- na.omit(Working_capital)
Working_capital <- Working_capital %>% spread(X1, value)
colnames(Working_capital)<- c("date","Total.Current.Assets" , "Total.Current.Liabilities")
Working_capital <- mutate(Working_capital, Working.Capital = Total.Current.Assets-Total.Current.Liabilities,
                          T.C.Liabilities.Over.T.C.Assets = Total.Current.Liabilities/Total.Current.Assets)

#Workingcapital <- cbind(Workingcapital,Current_Liabilites_over_Assets)

####################################################################### Operating Cash Flow/Sales Ratio
keywords <- CF.1$X1[grep("Cash from Operating Activities", CF.1$X1)]
OP_CF <- filter(CF.1, X1 %in% keywords)[-1] 
OP_CF <- OP_CF[-c(2),]
str(OP_CF)
OP_CF$value <- as.numeric(gsub(",","", OP_CF$value))
OP_CF <- na.omit(OP_CF)
colnames(OP_CF)<- c("date","Cash.from.Operating.Activities") 
EBITDA.1 <- inner_join(OP_CF,EBITDA.1,by="date")

Operating.Cash.Flow.Over.Sales.Ratio <- mutate(EBITDA.1, Operating.Cash.Flow.Over.Sales.Ratio = Cash.from.Operating.Activities/Revenue)
Matrix.Operating.Cash.Flow.Over.Sales.Ratio <- ROC.Function(Operating.Cash.Flow.Over.Sales.Ratio,Operating.Cash.Flow.Over.Sales.Ratio$Operating.Cash.Flow.Over.Sales.Ratio,Operating.Cash.Flow.Over.Sales.Ratio$date) 
K <- Matrix.Operating.Cash.Flow.Over.Sales.Ratio
Matrix.Operating.Cash.Flow.Over.Sales.Ratio <- as.data.frame(Matrix.Operating.Cash.Flow.Over.Sales.Ratio)
#Matrix.ROE <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Operating.Cash.Flow.Over.Sales.Ratio)[2:3] <- c(".Operating.Cash.Flow.Over.Sales.Ratio","%Change.of..Operating.Cash.Flow.Over.Sales.Ratio") 
str(Matrix.Operating.Cash.Flow.Over.Sales.Ratio) 


################################################################# Retained Earnings (Accumulated Deficit)

keywords <- BS.1$X1[grep("+ Retained Earnings", BS.1$X1)]
PRE_Retained <- filter(BS.1, X1 %in% keywords)[-1] 
PRE_Retained <- PRE_Retained[-c(1),]
str(PRE_Retained)
PRE_Retained$value <- as.numeric(gsub(",","", PRE_Retained$value))
colnames(PRE_Retained)<- c("date","Retained.Earnings") 


M <- list(EBITDA, Matrix.ROIC,Matrix.ROE,acccounts.payable,ShareholdersEquity,Matrix.Operating.Cash.Flow.Over.Sales.Ratio,PRE_Retained,Working_capital)
Altman <- Reduce(inner_join, M)
bin.table <-  cbind(Altman, Matrix.Enterprise.Value$date)
colnames(bin.table)[23] <- c("date")
colnames(bin.table)[1] <- c("dates")
str(bin.table)
L <- list(Matrix.Total.Assets, Matrix.Total.Equity, Matrix.Total.Debt,Matrix.Total.Liabilities,Matrix.DEBTEQUITY,Matrix.Total.Revenue,Matrix.Enterprise.Value
          ,Matrix.Short.term.debt.riskiness,Matrix.EBIT.Margin,Matrix.Net.Income.Margin,Matrix.Net.Changes.in.Cash)
Rate.of.Change <- Reduce(inner_join, L)
Rate.of.Change <- as.data.frame(Rate.of.Change)
str(Rate.of.Change)

Metrics <- inner_join(Rate.of.Change,bin.table,by="date")
str(Metrics)

##### Altman Z-scores below 1.23 indicate vulner- ability to bankruptcy, 
####scores between 1.23 and 2.90 are a gray area, and scores above 2.90 are considered safe.

#Altman_Table <- cbind(acccounts.payable,Total_Current_Assets, Sales, EBIT, WCAP,
#                      SHEQ, Total_Liabilities, Total_Assets, Retained_Earnings,Current_Liabilites_over_Assets)

Metrics <-  mutate(Metrics, Altman_Equation = 3.3*(EBIT/Total.Assets) + (Revenue/Total.Current.Assets) + .6*(Shareholders.Equity/Total.Liabilities)
                     + 1.4*(Retained.Earnings/Total.Assets) + 1.2*(Working.Capital/Total.Assets))

Matrix.Net.Altman.Equation <- ROC.Function(Metrics,Metrics$Altman_Equation,Metrics$date) 
K <- Matrix.Net.Altman.Equation
#Matrix.Net.Changes.in.Cash <- as.data.frame(Matrix.ROE)
Matrix.Net.Altman.Equation <- transform(K, value = c(NA, value[-nrow(K)]))
colnames(Matrix.Net.Altman.Equation)[2:3] <- c("Altman.Equation","ROC.Altman.Equation") 
str(Matrix.Net.Altman.Equation)
Matrix.Net.Altman.Equation <- Matrix.Net.Altman.Equation[-2]

Metrics <- inner_join(Metrics,Matrix.Net.Altman.Equation, by="date")

names(Metrics)
Metrics[, c(31,32,33,34,39,40)] <- sapply(Metrics[, c(31,32,33,34,39,40)], as.numeric)
str(Metrics)
#|%Change.of.Total.Assets|%Change.of.Total.Equity|%Change.of.Total.Debt|%Change.of.Total.Liabilities|%Change.of.Total.Revenue|%Change.of.Enterprise.Value|%Change.of.Short.term.debt.riskiness|%Change.of.EBIT.Margin|%Change.of.Debt.Equity|%Change.of.ROIC|%Change.of.ROE|%Change.of.Net.Income.Margin|%Change.of..Operating.Cash.Flow.Over.Sales.Ratio|%Change.of.Net.Changes.in.Cash
#|%Change.of..Operating.Cash.Flow.Over.Sales.Ratio
#dates|date
col_idx <- grep("%Change.of.Total.Assets|%Change.of.Total.Equity|%Change.of.Total.Debt|%Change.of.Total.Liabilities|%Change.of.Total.Revenue|%Change.of.Enterprise.Value|%Change.of.Short.term.debt.riskiness|%Change.of.EBIT.Margin|%Change.of.Debt.Equity|%Change.of.ROIC|%Change.of.ROE|%Change.of.Net.Income.Margin|%Change.of..Operating.Cash.Flow.Over.Sales.Ratio|%Change.of.Net.Changes.in.Cash|%Change.of..Operating.Cash.Flow.Over.Sales.Ratio", names(Metrics))
Metrics <- Metrics[, c(col_idx, (1:ncol(Metrics))[-col_idx])]


col_idx <- grep("dates|date", names(Metrics))
Metrics <- Metrics[, c(col_idx, (1:ncol(Metrics))[-col_idx])]

correlation <- na.omit(Metrics)
correlation <- cor(correlation[3:46])

colnames(Metrics)[3:16] <- c("ROC.Total.Assets", "ROC.Total.Equity","ROC.Total.Debt","ROC.Total.Liabilities", "ROC.Debt.Equity","ROC.Total.Revenue","ROC.Enterprise.Value","ROC.Short.term.debt.riskiness","ROC.EBIT.Margin", "ROC.Net.Income.Margin","ROC.Net.Changes.in.Cash",
                             "ROC.ROIC","ROC.ROE","ROC.Operating.Cash.Flow.Over.Sales.Ratio")



means <- colMeans(Metrics[3:46], na.rm = T, dims = 1)

dse = within(Metrics, {
  Altman.Z = ifelse(mean(na.omit(Metrics$Altman_Equation))  <= 1.8, 1, ifelse(
    mean(na.omit(Metrics$Altman_Equation)) > 3,5,0))
  DE.ratio = ifelse(mean(na.omit(Metrics$'%Change.of.Debt.Equity'))  >= 0, 1, 0)
  TA = ifelse(mean(na.omit(Metrics$'%Change.of.Total.Assets'))  >= 0, 5, 0)
  TE = ifelse(mean(na.omit(Metrics$'%Change.of.Total.Equity'))  >= 0, 5, 0)
  TD = ifelse(mean(na.omit(Metrics$'%Change.of.Total.Debt'))  >= 0, 5, 0)
  TL = ifelse(mean(na.omit(Metrics$'%Change.of.Total.Liabilities'))  >= 0, 1, 0)
  TR = ifelse(mean(na.omit(Metrics$'%Change.of.Total.Revenue'))  >= 0, 3, 0)
  #GP = ifelse(mean(na.omit(Metrics$GP_rates))  >= 0, 1, 0)
  SR.D = ifelse(mean(na.omit(Metrics$'%Change.of.Short.term.debt.riskiness'))  >= 0, -5, 0)
  NI = ifelse(mean(na.omit(Metrics$'%Change.of.EBIT.Margin'))  >= 0, 1, 0)
  EBIT.z = ifelse(mean(na.omit(Metrics$'%Change.of.Net.Income.Margin'))  >= 0, 1, 0)
  ROE = ifelse(mean(na.omit(Metrics$'%Change.of.ROE'))  >= 0, 1, 0)
  ROIC = ifelse(mean(na.omit(Metrics$'%Change.of.ROIC'))  >= 0, 1, 0)
  OP.CF = ifelse(mean(na.omit(Metrics$'%Change.of..Operating.Cash.Flow.Over.Sales.Ratio'))  >= 0, 1, 0)
  EV = ifelse(mean(na.omit(Metrics$'%Change.of.Enterprise.Value'))  >= 0, 1, 0)
})

#APPLE <- c(rowSums (dse[31:43], na.rm = T, dims = 1),colMeans(Overall.6[16:29], na.rm = T, dims = 1))
#GOOG <- c(rowSums (dse[31:43], na.rm = T, dims = 1),colMeans(Overall.6[16:29], na.rm = T, dims = 1))
micro <- c(rowSums (dse[47:58], na.rm = T, dims = 1),colMeans(Metrics[3:46], na.rm = T, dims = 1))

dse
##### Graphics
str(Metrics)
dff <- df[c(11:22),]
df <- na.omit(Metrics)
value <- c("FY.1993", "FY.1994", "FY.1995", "FY.1996", "FY.1997", "FY.1998",
         "FY.1999", "FY.2000", "FY.2001", "FY.2002", "FY.2003", "FY.2004", "FY.2005", "FY.2006",
         "FY.2007", "FY.2008", "FY.2009", "FY.2010", "FY.2011", "FY.2012", "FY.2013", "FY.2014",
         "FY.2015")

#df$dates <- data.frame("FY.1993", "FY.1994", "FY.1995", "FY.1996", "FY.1997", "FY.1998",
                            "FY.1999", "FY.2000", "FY.2001", "FY.2002", "FY.2003", "FY.2004", "FY.2005", "FY.2006",
                            "FY.2007", "FY.2008", "FY.2009", "FY.2010", "FY.2011", "FY.2012", "FY.2013", "FY.2014",
                            "FY.2015")

write.csv(df, "HLD.Metrics.df.csv")

dff <- HLD[c(13:22),]
HLD<- read.csv("HLD.Metrics.df.csv")
str(dff)

ggplot(dff, aes(date, y = Percent.Change.Value, color = Legend , group=1)) + 
  geom_line(aes(y = ROC.Short.term.debt.riskiness, col = "ROC.Short.term.debt.riskiness")) + 
  geom_line(aes(y = ROC.Altman.Equation, col = "ROC.Altman.Equation")) +
  geom_line(aes(y = ROC.Total.Debt , col = "ROC.Total.Debt")) +
  geom_line(aes(y = ROC.Operating.Cash.Flow.Over.Sales.Ratio , col = "ROC.Operating.Cash.Flow.Over.Sales.Ratio")) +
  #geom_line(aes(y = ROC.ROIC , col = "ROC.ROIC ")) +
  scale_shape_manual(values=c(date), guide = guide_legend(nrow=1)) +
  theme(axis.text.x=element_text(angle=60)) + ggtitle("The Percent Change of Financial Debt Ratios from 10 Years ") +
  theme_bw()
  
  

ggplot(Metrics, aes(x = date , y = EBITDA.Margin  , colour = EBITDA.Margin)) + 
  geom_line() + 
  ylab(label="Number of new members") + 
  xlab("Week Number")



