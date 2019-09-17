setwd("~/Mike_enquist_AQM")
#require(swirl)
library(car)

f1 <- function(pal){
  x<- read.csv(pal)
  x<- as.data.frame(x)
  return(x)
}

adv <- f1("Advertising.csv")[-1]
Sales <- adv$Sales
dat.adv <- adv
dat.adv <- adv[-4]
dat.adv.1 <- cbind(1,dat.adv)
dat.adv.2 <- data.matrix(dat.adv.1,rownames.force = NA)

dim(dat.adv)
head(dat.adv)
tail(dat.adv)
nrow(dat.adv)
summary(dat.adv)
colMeans(dat.adv)



# trying to find a way to fit the model
pairs(dat.adv)
model <- lm(Sales ~ TV^2 + sqrt(Radio) + poly(Newspaper,4) , data = adv)
summary(model)
anova(model)

plot((model))

crPlots(model)

#### We can verify this by doing going through the 4 assumptions 

##### First Assumption: We want the Expected(residuals)=0. 

plot(residuals(model))

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

plot(fitted(model), residuals(model), xlab="Fitted", ylab= "residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
plot(fitted(model), abs(residuals(model), xlab= "Fitted", main="|residuals|")
     summary(lm(abs(residuals(model) ~ fitted(model)))
             qqnorm(residuals(model), ylab="residuals")
             qqline(residuals(model), col= "red", lwd = 2)  
             hist(residuals(model))
             
             predict(model) 
             pred <- predict(Z, newdata = x.test)
             obs <- x.test$y
             plot(obs ~ pred)
             abline(c(0,1), col="red", lwd = 2)
             RVSE <- sqrt(mean(pred - obs)^2)
             AE <- sum(abs(pred - obs))
             acf(pred)
             pacf(pred)
             summary(pred)
             


#transforming the fucntion from the residuals using matrix notation.

X<- dat.adv.2
Y<- Sales
beta <- solve(t(X)%*%X)%*%t(X)%*%Y  
beta

residuals <- (Y - X %*% beta)


# Plot residuals by using ggplot
library(ggplot2)
residuals.TV <- ggplot(adv,aes(TV,residuals))
residuals.Radio <- ggplot(adv,aes(Radio,residuals))
residuals.Newspaper <- ggplot(adv,aes(Newspaper,residuals))



  residuals.TV+geom_point()+geom_hline(yintercept = 0, col = "blue", lwd = 1)+
  labs(title = "TV residuals")
  labs(y = "Residuals")


  residuals.Radio+geom_point()+geom_hline(yintercept = 0, col = "green", lwd = 1)+
  labs(title = "Radio Residuals")+
  labs(y = "Residuals")
  
  residuals.Newspaper+geom_point()+geom_hline(yintercept = 0, col = "purple", lwd = 1)+
  labs(title = "Newspaper Residuals")+
  labs(y = "Residuals")
            
             
  # what i have found out that TV and Radio had the similar pattern and there residuals where closer to the line
  #compared to newspaper. If a transformation is necessary,then start by transforming the newspaper variable because the 
  #results of the model will be easy to understand.
  
  
  #excercise on slide 33           
  
  
  E <- vector()
  for(i in 1:1000){
    E[i] <- rnorm(1)  
  }
  
  X <- 1:1000
  beta <- .25
  y = X * beta + E
  
  plot(y)
  
  hist(E)
  
  reg <- lm(y~X) 
  
  plot(predict(reg),resid(reg))
  