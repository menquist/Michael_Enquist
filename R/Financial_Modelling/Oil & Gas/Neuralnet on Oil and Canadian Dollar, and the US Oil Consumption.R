#setwd("~/R_Stuff/Stock_Analysis/")

# dependencies
suppressPackageStartupMessages({
  library(foreach)
  library(reshape2)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(quantmod)
  options("getSymbols.warning4.0"=FALSE)
  library(knitr)
  library(rjson)
  library(Quandl)
})

Quandl.api_key("c58SRnkLsYYrxJs6d74e")

### Importing and cleaning Data
oil.price <- Quandl("ODA/POILWTI_USD", collapse="monthly",start_date="2000-01-01")

US.interest.rate <- Quandl("FED/SVENPY",  collapse="monthly",start_date="2000-01-01")
US.interest.rate <- US.interest.rate[1:2]

CAD.USA.EX <- Quandl("BOE/XUMLCDD",collapse="monthly", start_date="2000-01-01")

US.OIL.Imports <- Quandl("FRED/IR10010",collapse="monthly",start_date="2000-01-01")

#CAD.US <- getSymbols("DEXCAUS", src="FRED", auto.assign = FALSE)
#CADUSEX<-data.frame(date=as.character(index(CAD.US)),coredata(CAD.US))
#CADUSEX <- na.omit(CADUSEX)

names(oil.price) <- c("Date", "Oil.Price")
names(US.interest.rate) <- c("Date", "Interest.Rate")
names(CAD.USA.EX) <- c("Date", "CAD.USA")
names(US.OIL.Imports) <- c("Date", "Oil.Imports")


newdata <- merge(oil.price,US.interest.rate, by="Date", all.y = T)
data <- as.data.frame(inner_join(newdata, CAD.USA.EX, by = "Date")) 
data <- as.data.frame(inner_join(data, US.OIL.Imports, by = "Date")) 

data <- na.omit(data)

test <- data[2:5]
n <- names(test)
f <- as.formula(paste("Oil.Price ~", paste(n[!n %in% "Oil.Price"], collapse = " + ")))
f
# Reset R!!!
#Neural Net!!!!
#Train the neural network
#Going to have 3 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
library(neuralnet)

Oil <- neuralnet(f, data=data, hidden = 3, threshold = .01,
                  linear.output = T)
plot(Oil)
Oil$result.matrix
Oil$weights
Oil$covariate
Oil$response
Oil$linear.output
Oil$net.result[[1]]
Oil1 = ifelse(Oil$net.result[[1]]>0.5,1,0)
Oil1


### Using backprop algorithm to see is if the model is better
nn.Oil <- neuralnet(f, data=test, hidden = 3,
                     learningrate = .01,err.fct="sse", algorithm = "backprop"
                     ,linear.output = F)
pred <- print(prediction(nn.Oil))
plot(nn.Oil)
#################################################################

#### GLM Model
data <- data[2:5]

set.seed(500)
library(MASS)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(f, data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Oil.Price)^2)/nrow(test)

############ Neural net test part 2!
apply(data,2,function(x) sum(is.na(x)))
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

nn <- neuralnet(f, data=train_, hidden = 3,
                learningrate = .01,err.fct="sse", algorithm = "backprop"
                ,linear.output = F)
plot(nn)
nn$result.matrix
##### Proof that Neural net is better than GLM

pr.nn <- compute(nn,test_[,2:4])

pr.nn_ <- pr.nn$net.result*(max(data$Oil.Price)-min(data$Oil.Price))+min(data$Oil.Price)
test.r <- (test_$Oil.Price)*(max(data$Oil.Price)-min(data$Oil.Price))+min(data$Oil.Price)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#### Now we compare two MSE's
print(paste(MSE.lm,MSE.nn))

########## Plots comparing NN VS GLM 
par(mfrow=c(1,2))

plot(test$Oil.Price,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$Oil.Price,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)



#So far, the neuralnet is fitting better than the linear regression
plot(test$Oil.Price,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$Oil.Price,pr.lm,col='blue',pch=18,cex=0.7)
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
  test.cv.1 <- test.cv[2:4]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  pr.nn <- compute(nn,test.cv.1)
  pr.nn <- pr.nn$net.result*(max(data$Oil.Price)-min(data$Oil.Price))+min(data$Oil.Price)
  
  test.cv.r <- (test.cv$Oil.Price)*(max(data$Oil.Price)-min(data$Oil.Price))+min(data$Oil.Price)
  
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

#By going through this model, the average MSE for the neural network slightly smaller than  
#the linear model. Neural networks resemble black boxes so we should not fully interpret this model
# until we make further investigation. 














