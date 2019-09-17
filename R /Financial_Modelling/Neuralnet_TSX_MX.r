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

library(clusterGeneration)
require(MASS)
seed.val<-2
set.seed(seed.val)

num.vars<-8
num.obs<-1000

#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1,y2)
names(resp)<-c('Y1','Y2')
dat.in<-data.frame(resp,rand.vars)

#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in<-as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2<-neuralnet(form.in,data=dat.in,hidden=10)

#mlp function from RSNNS package
library(RSNNS)
library(Rcpp)
set.seed(seed.val)
mod3<-mlp(rand.vars, resp, size=10,linOut=T)

#import the function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


#plot each model
plot.nnet(mod1)
plot.nnet(mod2)
plot.nnet(mod3)

wts.in<-mod1$wts
struct<-mod1$n
plot.nnet(wts.in,struct=struct)

mod.in<-c(13.12,1.49,0.16,-0.11,-0.19,-0.16,0.56,-0.52,0.81)
struct<-c(2,2,1) #two inputs, two hidden, one output 
plot.nnet(mod.in,struct=struct)










##########Ticker data    ...................

############################################################

setwd("~/R_Stuff/Stock_Analysis/TSX.MX-Tickers/")
SP <-  getSymbols(Symbols = "^GSPTSE", src = "yahoo", auto.assign = FALSE )
SP<-data.frame(date=as.character(index(SP)),coredata(SP))
SP.adj <- SP %>% 
  select(date,GSPTSE.Adjusted) %>% 
  mutate(GSPTSE = 100*((GSPTSE.Adjusted - lag(GSPTSE.Adjusted)))/GSPTSE.Adjusted)
SP.adj <- na.omit(SP.adj[-2])

temp = list.files(pattern="*.csv")
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names =T )
  datalist = try(lapply(temp,function(x){read.csv(file=x,header=T,colClasses=c("NULL", NA, NA))}))
  try(Reduce(function(x,y) {merge(x, y, by="date", all.x=TRUE)}, datalist))
}

mydata <- multmerge("")

#col.names = colnames(read.table(file ="headers_28_subset.txt", header = T))
col.names = temp
names(mydata)[2:99]=col.names

#valid_column_names <- make.names(names=names(mydata), unique=TRUE, allow_ = TRUE)
#names(mydata) <- valid_column_names
#### choose tickers!!!!
#date <- mydata$date
#mydata.1  <- mydata
#mydata <- cbind(date,mydata.1)
#######################
combined <- sort(union(levels(SP.adj$date), levels(mydata$date)))
n <- left_join(mutate(SP.adj, a=factor(date, levels=combined)),
               mutate(mydata, a=factor(date, levels=combined)))
n <- n[-3]
mydata <- na.omit(n)


companies <- mydata %>% 
  select(SU.TO.csv,MFC.TO.csv,SLF.TO.csv,ECA.TO.csv,PSK.TO.csv,LUN.TO.csv)
companies <- na.omit(companies)
colnames(companies) <- NULL
str(companies)

write.csv(companies, "TSX_NNET.csv")


#### For GMLNET
test <- xts(mydata[,-1], order.by=as.Date(mydata$date))

test <- n[2:100]
#######

n <- names(test)
f <- as.formula(paste("GSPTSE ~", paste(n[!n %in% "GSPTSE"], collapse = " + ")))
f

library(neuralnet)
library(BASS)
Bank <- neuralnet(f, data=test, hidden = c(10,5,2,1), threshold = .01,
                  linear.output = F)
Bank$result.matrix
Bank$weights
Bank$covariate
Bank$response
Bank$linear.output
Bank$net.result[[1]]

Bank1 = ifelse(Bank$net.result[[1]]>0.5,1,0)
Bank1

nn.bank <- neuralnet(f, data=test, hidden = 10,
                     learningrate = .01, algorithm = "backprop"
                     ,linear.output = FALSE)
pred <- print(prediction(nn.bank))
nn.bank$result.matrix

#################################################################################################
#### GLM Model
data <- mydata[2:100]

library(MASS)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(f, data=train)
summary(lm.fit)
plot(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$GSPTSE.Adjusted)^2)/nrow(test)

############ Neural net test part 2!
apply(data,2,function(x) sum(is.na(x)))
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

nn <- neuralnet(f, data=test, hidden = c(99), threshold = .01,
                           linear.output = F)

summary(nn)
nn$result.matrix
##### Proof that Neural net is better than GLM
test.1 <- test_[2:99]
pr.nn <- compute(nn,test.1)

pr.nn_ <- pr.nn$net.result*(max(data$GSPTSE.Adjusted)-min(data$GSPTSE.Adjusted))+min(data$GSPTSE.Adjusted)
test.r <- (test_$GSPTSE.Adjusted)*(max(data$GSPTSE.Adjusted)-min(data$GSPTSE.Adjusted))+min(data$GSPTSE.Adjusted)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#### Now we compare two MSE's
print(paste(MSE.lm,MSE.nn))

########## Plots comparing NN VS GLM 
par(mfrow=c(1,2))

plot(test$GSPTSE.Adjusted,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$GSPTSE.Adjusted,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$GSPTSE.Adjusted,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$GSPTSE.Adjusted,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))



#We are going to implement a fast cross validation using a for loop for the neural network 
#and the cv.glm() function in the boot package for the linear model.
library(boot)
set.seed(200)
lm.fit <- glm(f,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

#I am splitting the data in this way: 90% train set and 10% test set in a random way for 10 times.
#I am also initializing a progress bar using the plyr library because I want to keep an eye on the status of the
#process since the fitting of the neural network may take a while.
set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  test.cv.1 <- test.cv[2:99]
  
  nn <- neuralnet(f, data=test, hidden = c(99), threshold = .01,
                  linear.output = F)
  pr.nn <- compute(nn,test.cv.1)
  pr.nn <- pr.nn$net.result*(max(data$GSPTSE.Adjusted)-min(data$GSPTSE.Adjusted))+min(data$GSPTSE.Adjusted)
  
  test.cv.r <- (test.cv$GSPTSE.Adjusted)*(max(data$GSPTSE.Adjusted)-min(data$GSPTSE.Adjusted))+min(data$GSPTSE.Adjusted)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
  
}

#### the average MSE for the neural network (10.33) is lower than the one of the linear model although 
#there seems to be a certain degree of variation in the MSEs of the cross validation
mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)



library(nnet)
library(caret)
library(forecast)
fit  <- avNNet(SP.adj$GSPTSE~model, data=model, repeats=99, size=3, decay=0.1,
               linout=TRUE)

fit <- nnetar(model)
fcast <- forecast(fit)
plot(fcast)
summary(fcast)
fcast$fitted


####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
# GLMNET
library(glmnet)
test <- na.omit(test)

# random select
#tsx.stock <- test[c(1,28,83,64)]
#tsx.stock <- na.omit(tsx.stock)
#TSX <- tsx.stock[1]
# Logistic Indicator
#data_logit <- tsx.stock %>%
#  mutate(long = ifelse(GSPTSE < 0, 0, 1)) %>%
#  mutate(short = ifelse(GSPTSE > 0, 0, 1))
#tsx.stock  <- tsx.stock[-1]

tsx.stock <- as.matrix(tsx.stock)
TSX <- data_logit[6]


# Logistic Indicator
data_logit <- mydata %>%
  mutate(long = ifelse(GSPTSE < 0, 0, 1)) %>%
  mutate(short = ifelse(GSPTSE > 0, 0, 1))



TSX <- data_logit[100]
tsx.stock <- data_logit[2:99]

#running glmnet having problems with binomail but poisson is working
model2 <- glmnet(as.matrix(tsx.stock),y=as.matrix(TSX), family="binomial", alpha = 1)
summary(model2)

coef<-predict(model2, type="coefficients")
lasso <- cv.glmnet(as.matrix(tsx.stock),as.matrix(TSX), family="binomial") 
lasso_coef<-predict(lasso, type="coefficients")
plot(model2, label = TRUE)
plot(lasso, label =T )
plot(coef, label=T)
plot(lasso_coef, label=T)




model2 <- glmnet(as.matrix(TSX),as.integer(tsx.stock), family= "poisson")




setwd("~/Downloads/Mike_enquist_AQM-master/Workshop Materials/week 6")
newsdata <- read.csv("OnlineNewsPopularity.csv", header = T)

str(newsdata)

#omit url
newsdataomit <- newsdata[-1]
str(newsdataomit)

#glm Logistic regression ####################################################
library(aod)



sapply(newsdataomit, sd)

newsdataomit$shares <- factor(newsdataomit$shares)
mylogit <- glm(shares ~., data = newsdataomit, family = "binomial")
head(newsdataomit$shares)

newsdataomit$shares <- as.numeric(newsdataomit$shares)

summary(mylogit)


data <- newsdataomit %>%
  mutate(rateofchange = (shares - lag(shares))/shares) %>% 
  na.omit(data)

str(data)

# Logistic Indicator
data_logit <- data %>%
  mutate(long = ifelse(rateofchange < 0, 0, 1)) %>%
  mutate(short = ifelse(rateofchange > 0, 0, 1))







index <- read.csv("data_output/DJIndexOfVentureCapital-2010Q2.csv")
ff <- read.csv("data_output/DJIndexOfVentureCapital-2010Q2-ff.csv")
reg <- lm(X2010.Q2 ~ X2010.Q1.Codexis.Fixed,data = index)
summary(reg)

reg.ff <- lm(VC_RET ~ RF+VC_RET.RF+Mkt.RF+SMB+HML+LF,data = ff)
summary(reg.ff)
anova(reg.ff)
plot(reg.ff)
pairs(ff)
abline(coef(Brazilregress), col = "blue")

library(neuralnet)

curr <- neuralnet(VC_RET ~ RF+VC_RET.RF+Mkt.RF+SMB+HML+LF,data = ff, hidden = 2,
                  linear.output = FALSE)
plot(curr)
curr$result.matrix
curr$weights
curr$covariate
curr$net.result[[1]]
curr11 = ifelse(curr$net.result[[1]]>0.5,1,0)
curr11

