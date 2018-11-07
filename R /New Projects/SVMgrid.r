library(caret)
library(dplyr)
library(doMC)
registerDoMC(cores=4)
set.seed(1345)
dat <- twoClassSim(2000)

svmGrid <- expand.grid(sigma= 2^c(-25, -20, -15,-10, -5, 0), C= 2^c(0:5))

set.seed(45)
mod <- train(Class ~ ., data = dat, 
             method = "svmRadial",
             preProc = c("center", "scale"),
             tuneGrid = svmGrid,
             metric = "ROC",
             trControl = trainControl(method = "cv", 
                                      classProbs = TRUE, 
                                      summaryFunction = twoClassSummary))

tmp <- mod$results
tmp$sigma2 <- paste0("2^", format(log2(tmp$sigma)))

xyplot(ROC ~ C, data = tmp, 
       groups = sigma2,
       type = c("p", "l"),
       auto.key = list(columns = 4, lines = TRUE),
       scales = list(x = list(log = 2)),
       xlab = "Cost", 
       ylab = "ROC (Cross-Validation)")


test_pred <- predict(mod, newdata = dat)
test_pred

str(testing)
confusionMatrix(test_pred, dat )


######  @ Argimiro Arratia, 2014,   NNET and SVM modeling
###### http://computationalfinance.lsi.upc.edu

#wdir="~/the path to the data"
#setwd(wdir)

########Visual Test of normality#################
require(quantmod)
appl = readRDS("AAPL")
apRd= periodReturn(appl,period="daily")
dsd=density(apRd) #estimate density of daily log ret
yl=c(min(dsd$y),max(dsd$y)) #set y limits
hist(apRd,probability=T,xlab="APPLE returns",main=NULL,ylim=yl)
lines(dsd)
##plot the normal density with mean, stdv of apRd
a=seq(min(apRd),max(apRd),0.001)
lines(a,dnorm(a,mean(apRd),sd(apRd)),col="red")

##Repeat above with period="weekly", "monthly".
##Run a Shapiro-wilk normality test
shapiroTest(apRd)
##############################################
