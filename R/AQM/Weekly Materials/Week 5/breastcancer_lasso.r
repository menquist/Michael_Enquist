setwd("~/github/Enquist_Mike/Enquist_Mike/AQM Assignments")

df <- na.omit(read.csv("BreastCancer.csv", header = TRUE))[-1]
str(df)


obs <- 683
Class <- df$Class
dat.adv <- df[-10]
dat.adv.1 <- cbind(1,dat.adv)
dat.adv.1 <- dat.adv.1
dat.adv.2 <- data.matrix(dat.adv.1,rownames.force = NA)

y<- length(Class)
X<- dat.adv.2
Y<- Class
beta <- solve(t(X)%*%X)%*%t(X)%*%Y  
beta

residuals <- (Y - X %*% beta)

#Calculate the first derivative of likelihood function given output (y) , input (x) and pi (estimated probability)
calculateder <- function(y,x,pi) {
  derv <- y*x - pi*x
  derv_sum <- sum(derv)
  return(derv_sum)
}

#Calculate the likelihood function given output(y) and pi
calculatell <- function(y,pi) {
  ll <- 1
  ll_unit <- 1:length(y)
  for (i in 1:length(y)){
    ll_unit[i] <- ifelse(y[i] == 1,pi[i],1-pi[i])
    ll = ll_unit[i]*ll
  }
  return(ll)
}

#Calculate the value of pi (predictions on each observation) given x_new(input) and estimated betas
findpi <- function(x_new,beta){
  pi <- 1:nrow(x_new)
  expon <- 1:nrow(x_new)
  for (i in 1:nrow(x_new)){
    expon[i] <- 0
    for (j in 1:ncol(x_new)){
      expo <- x_new[i,j] * beta[j]
      expon[i] <- expo + expon[i]}
    pi[i] <- exp(expon[i])/(1+exp(expon[i]))
  }
  return(pi)
}

#Calculate the matrix W with all diagnol values as pi 
findW <- function(pi){
  W <- matrix(0,length(pi),length(pi))
  for (i in 1:length(pi)){
    W[i,i] <- pi[i]*(1-pi[i])
  }
  return(W)
}


# Lets now make the logistic function given list of required inputs
logistic <- function(x,y,vars,obs,learningrate,dif) {
  beta <- rep(0, (vars+1))
  bias <- rep(1, obs)
  x_new <- cbind(bias,x)
  derivative <- 1:(vars+1)
  diff <- 10000
  while(diff > dif) {
    pi <- findpi(x_new,beta)
    pi <- as.vector(pi)
    W <- findW(pi)
    derivative <- (solve(t(x_new)%*%W%*%as.matrix(x_new))) %*% (t(x_new)%*%(y - pi))
    beta = beta + derivative
    diff <- sum(derivative^2)
    ll <- calculatell(y,pi)
    print(ll)
  }
  return(beta)
}

# Time to test our algorithm with the values we mentioned at the start of the article
x <- Y
y <- adv$TV
a <- logistic(x,y,1,10,0.01,0.000000001)
calculatell(y,findpi(x_new,a))
#Log Likelihood = 0.01343191
data <- cbind(x,y)
data <- as.data.frame(data)
mylogit <- glm(y ~ x, data = data, family = "binomial")
mylogit
preds <- predict(mylogit, newdata = data,type ="response")
calculatell(data$y,preds)
#Log Likelihood = 0.01343191
#Isn't this amazing!!!


#running pairs function 
pairs(bus_path)

#histogram
hist(bus_path$ArriveDelay , breaks = 100)

dse = within(new_my_data, {
  Arrived = ifelse(ArriveDelay < 0, 0, 1)
  Delayed = ifelse(ArriveDelay > 0, 0, 1)
})

na.omit(dse)
dse <- as.matrix(dse)
m <- as.data.frame(dse)
library(glmnet)
model2 <- glm(Delayed~.,data=m, family=binomial())

Part.2 <-  filter.integers.numerics %>% 
  select(ArriveLoad,LeaveLoad, Ons,ArriveLoadCompensated,OnsLoadCompensated,
         VehicleAge,VehicleYear,VehicleCapacity,
         VehicleSittingCapacity,Conditions,
         Wind.Speed,Visibility,Humidity,Temp,DwellTime,DepartureDelay,
         ArriveDelay) 


new_my_data.1 <- dummy.data.frame(Part.2, names = c("Pattern", "Line","TimingPoint",
                                                    "WCLiftActivated","ActualLeaveTime","ScheduledLeaveTime",
                                                    "ActualArriveTime","ScheduledArriveTime", "VehicleNo","DayType",
                                                    "BikeLoaded","BikeUnloaded","VehicleDescription","VehicleClass",
                                                    "VehicleFuelTyle","VehicleLength","VehicleCapacity","VehicleSittingCapacity","Conditions",
                                                    "StopName","Hour"))
str(new_my_data.1)

which(apply(new_my_data.1, 2, var)==0)


dse = within(new_my_data.1, {
  Arrived = ifelse(ArriveDelay < 0, 0, 1)
  Delayed = ifelse(ArriveDelay > 0, 0, 1)
})

na.omit(dse)
dse <- as.matrix(dse)
m <- as.data.frame(dse)
model4 <- glm(Delayed~.,data=m, family=binomial()) 


# Logistic Indicator
data_logit <- new_my_data.1 %>%
  mutate(Delayed = ifelse(ArriveDelay < 0, 0, 1)) %>%
  mutate(Early = ifelse(ArriveDelay > 0, 0, 1))

compensated <- data_logit$Early
bus_path <- new_my_data.1[-24]
str(bus_path)

#running glmnet having problems with binomail but poisson is working
model2 <- glmnet(as.matrix(bus_path),y=as.matrix(compensated), family="binomial")
summary(model2)

coef<-predict(model2, type="coefficients")
coef
lasso <- cv.glmnet(as.matrix(bus_path),as.matrix(compensated), family="binomial") 

lasso_coef<-predict(lasso, type="coefficients")
plot(model2, label = TRUE)
plot(lasso, label =T )
plot(coef, label=T)
plot(lasso_coef, label=T)

m2<-lm(ArriveDelay~.,data=new_my_data.1)
summary(m1)

dse <- as.matrix(new_my_data.1)
m <- as.data.frame(dse)

