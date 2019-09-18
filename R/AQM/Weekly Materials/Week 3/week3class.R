Protein <- read.csv("gpro.txt", header = F, col.names = c("x1", "x2", "x3", "x4", "y"))
dim(Protein)
summary(Protein)
str(Protein)
train <- Protein[1:4000,]
test <- Protein[4001:14000,]
model <- lm(y ~ x1 + x2 + x3 + x4, data=Protein)
summary(model) 
Trainx <- train[sample.int(dim(train)[1], size=100), ]
Testx <- test[sample.int(dim(test)[1], size=1000), ]
dim(Trainx)      
dim(Testx)
install.packages("plyr")
install.packages("dplyr")
x.train <- sample_n(train,100)
x.test <- sample_n(test, 1000)

#Running a regression for train
mod1 <- lm(y ~ x1 + x2 + x3 + x4, data=Trainx)
summary(mod1)
anova(mod1)
#Running the regression for test
mod2 <- lm(y ~ x1 + x2 + x3 + x4, data=Testx)
summary(mod2)
anova(mod2)
plot(residuals(mod1) ~ Trainx$x1)
plot(residuals(mod1) ~ Trainx$x2)
plot(residuals(mod1) ~ Trainx$x3)
plot(residuals(mod1) ~ Trainx$x4)
#run cbind for trainx
norm <- rnorm(100)
plot(norm)
plot(cbind(residuals(mod1), Trainx[,1:4]), lower.panel = NULL)
install.packages("car")
library(car)
library(plyr)
library(dplyr)
crPlots(mod1)
plot(cbind(residuals(mod1), Trainx[,1:4]), lower.panel = NULL)
plot(fitted(mod1), residuals(mod1), xlab="Fitted", ylab= "residuals"
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )     
plot(fitted(mod1), abs(residuals(mod1), xlab= "Fitted", main="|residuals|")
summary(lm(abs(residuals(mod1) ~ fitted(mod1)
qqnorm(residuals(mod2), ylab="residuals")
qqline(residuals(mod2), col= "red", lwd = 2)  
hist(residuals(mod2))
x.test <- sample_n(test, 100)     
plot(residuals(mod1))
acf(residuals(mod1))
pacf(residuals(mod1))

predict(mod1) 
pred <- predict(mod1, newdata = x.test)
obs <- x.test$y
plot(abs ~ pred)
abline(c(0,1), col="red", lwd = 2)
RVSE <- sqrt(mean(pred - obs)^2)
AE <- sum(abs(pred - obs))
acf(pred)
pacf(pred)

