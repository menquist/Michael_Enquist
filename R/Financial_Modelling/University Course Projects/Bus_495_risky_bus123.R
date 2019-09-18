setwd("~/Downloads")

$#plotmarket <- read.csv("Marketplot_Risk123.csv", header = T)
f1 <- function(pal){
  x<- read.csv(pal)
  x<- as.data.frame(x)
  return(x)
} 

Marketplot <- f1("Marketplot_Risk123.csv")[-1]
str(Marketplot)
Marketplot <- Marketplot[-7]

model_Risk123 <-lm(riskybusiness123~ ., data=Marketplot)
model_Risk123
summary(model_Risk123)
plot(model_Risk123)
pairs(Marketplot)

crPlots(model_Risk123)

model <- model_Risk123

#### We can verify this by doing going through the 4 assumptions 

##### First Assumption: We want the Expected(residuals)=0. 

plot(residuals(model))
qqline(residuals(model), col="red", lwd = 2)
###### There are some strong negative residuals. However, overall, there the residuals are centered. We can also plot this using the car package using crPlot(mod) which will use the partial residuals

##### Second Assumption: Constant Variance. 
###### We want the variance of the residuals to be around zero 

plot(fitted(model), residuals(model),xlab="Fitted", ylab="Residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red", "red", "red"), lty = c(2,1,2))

###### According to our graph, there is large variance from zero.

##### Third Assumption: Normality of Errors 

qqnorm(residuals(model) ,ylab="Residuals")
qqline(residuals(model), col="red", lwd = 2)
hist(residuals(model))


##### Fourth Assumption: Uncorrelated Errors 

plot(residuals(model))
acf(residuals(model))
pacf(residuals(model))

crPlots(model)


################# training test
Marketplot
train <- Marketplot[1:200,]
test <- Marketplot[201:728,]

Trainx <- train[sample.int(dim(train)[1], size=200), ]
Testx <- test[sample.int(dim(test)[1], size=528), ]
dim(Trainx)      
dim(Testx)

x.train <- sample_n(train,200)
x.test <- sample_n(test, 528)
dim(x.train)
dim(x.test)


model <-lm(riskybusiness123~ poly(ent6,3) + poly(allstar,3)+ poly(rexjeff,3)+ipoguaranteed+iscreate, data=Marketplot)
summary(model)

plot(fitted(model), residuals(model), xlab="Fitted", ylab= "residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
plot(fitted(model), abs(residuals(model)), xlab= "Fitted", main="|residuals|")
     summary(lm(abs(residuals(model)) ~ fitted(model)))
             qqnorm(residuals(model), ylab="residuals")
             qqline(residuals(model), col= "red", lwd = 2)  
             hist(residuals(model))
             
             predict(model) 
             pred <- predict(model, newdata = x.test)
             obs <- x.test$riskybusiness123
             plot(obs ~ pred)
             abline(c(0,1), col="red", lwd = 2)
             RVSE <- sqrt(mean(pred - obs)^2)
             AE <- sum(abs(pred - obs))
             acf(pred)
             pacf(pred)
             summary(pred)


######################################  VC

plotmarket <- read.csv("Market_plot_VC.csv", header = T)
f1 <- function(pal){
  x<- read.csv(pal)
  x<- as.data.frame(x)
  return(x)
} 

Market.size <- f1("Market_plot_VC.csv")[-1]

str(Market.size)

############### regression on firms


Market.Plot.VC2 <- f1("Market_Share_VC2.csv")



VC2.Mkt.Share <- cbind(Market.size,Market.Plot.VC2)
str(VC2.Mkt.Share)

pairs(VC2.Mkt.Share)


model_regression_Mrk.share <-lm(Daily.market.size.in.thousand.dollars.per.month~., data=VC2.Mkt.Share)
model_regression_Mrk.share
summary(model_regression_Mrk.share)
anova(model_regression_Mrk.share)
plot(model_regression_Mrk.share)


Market.size <- f1("Market_plot_VC.csv")[-1]
# Subseting dataframe

Market.Plot.VC2 <- f1("Market_Share_VC2.csv")[1:300,]
Market.size <- Market.size[1:300,]

VC2.Mkt.Share.1 <- cbind(Market.size,Market.Plot.VC2)
str(VC2.Mkt.Share.1)

model_regression_Mrk.share.1 <-lm(Market.size~., data=VC2.Mkt.Share.1)
summary(model_regression_Mrk.share.1)
anova(model_regression_Mrk.share.1)
plot(model_regression_Mrk.share.1)


########################################################## Vaultdud

Vaultdud.cf <- f1("Vaultdud.cf.csv")
Market.size <- f1("Market_plot_VC.csv")[-1]

Vaultdud.cf  <- as.numeric(levels(Vaultdud.cf))
breast <- transform(Vaultdud.cf, class=as.numeric(as.character(breast)))
str(Vaultdud.cf)

VC2.Mkt.Share.2 <- cbind(Vaultdud.cf,Market.size)
Market.size <- Market.size[-1,]
str(VC2.Mkt.Share.2)
VC2.Mkt.Share.2 <- cbind(Vaultdud.cf,Market.size)



model_regression_Mrk.share.2 <-lm(Market.size~., data=VC2.Mkt.Share.2)
summary(model_regression_Mrk.share.2)
anova(model_regression_Mrk.share.2)
plot(model_regression_Mrk.share.2)





