library(MASS)
library(quantmod)
#load a few symbols into memory
stocks <- getSymbols(c("^GSPC", "^NYA", "^GSPTSE"))
SP<-data.frame(date=as.character(index(stocks)),coredata(stocks))
#plot the prices of these stocks 
#par(mfrow = c(3,2))
plot(AC.TO[,6], main = "AC.TO")
#plot(XFN.TO[,6], main = "QQQQ")
#plot(SPY[,6], main = "SPY")
#plot(GOOG[,6], main = "GOOG")
#plot(CVX[,6], main = "CVX")
#par(mfrow = c(1,1))

#compute price matrix
pM <- na.omit(cbind(GSPC[,6], GSPTSE[,6], NYA[,6]))

pM <- na.omit(mydata)
pM <- pM[-1]
#compute returns matrix
rM <-  apply(pM,2,function(x) diff(log(x)))

#look at pairwise charts
#pairs(coredata(rM))

#compute the covariance matrix
covR <- cov(rM)

#use this covariance matrix to simulate normal random numbers
#that share a similar correlation structure with the actual data
meanV <- apply(rM, 2, mean)
rV    <- mvrnorm(n = nrow(rM), mu = meanV, Sigma = covR)

#simulate prices based on these correlated random variables

#calculate mean price
p0 <- apply(pM,2,mean)
sPL <- list()
for(i in 1:ncol(rM)){
  sPL[[i]] <-round(p0[i]*exp(cumsum(rV[,i])),2)
}

#plot simulated prices
par(mfrow = c(3,2)) 
plot(sPL[[1]],main="1",type="l")
plot(sPL[[2]], main = "2 sim",type = "l")
plot(sPL[[3]], main = "3 sim", type = "l") 
plot(sPL[[16]], main = "4 sim",type = "l") 
plot(sPL[[11]], main = "5 sim", type = "l")
plot(sPL[[12]], main = "6 sim", type = "l")
