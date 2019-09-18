###############
<<<<<<< HEAD
setwd("~/Mike_enquist_AQM")
adv <- read.csv("seeds_dataset.csv")
## Working with the code given to us at AQM 


=======


XM <- data.matrix(X, rownames.force = NA)
XMT <- t(XM)
I <- solve(XMT%*%XM)
B <- I%*%XMT%*%Y
SSE <- t(Y-(XM%*%B))%*%(Y-(XM%*%B))
Residuals <- Y-XM%*%B

SSR  <- function(X,Y) {
  XM <- data.matrix(X, rownames.force = NA)
  XMT <- t(XM)
  I <- solve(XMT%*%XM)
  B <- I%*%XMT%*%Y
  SSE <- t(Y-(XM%*%B))%*%(Y-(XM%*%B))
  Residuals <- Y-XM%*%B
  return(SSE)
}


SSR <- function(X,Y)
{
  X<-cbind(1,X)
  XM <- data.matrix(X, rownames.force = NA)
  XMT <- t(XM)
  I <- solve(XMT%*%XM)
  B <- I%*%XMT%*%Y
  SSE <- t(Y-(XM%*%B))%*%(Y-(XM%*%B))
  return(SSE)
}

#test
SSR(adv[,1:3],adv[,4])

ssquares <- function(x){
  n <- nrow(adv)
  sum((adv[,4] - cbind(1, adv[,1])) %*% x)^2 /n
}


derivative <- function(x){
  
  n <- nrow(adv) #200
  c(sum(-2*(adv[,4]-cbind(1, adv[,-1]) %*% X)), sum(-2*(adv[,-1])*(adv[,4]-cbind(1, adv[,1]) %*% X))) / n
}



gradient.descent <- function(func, derv, start, step=.00001, tot=1e-8)
{
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  print(func(pt1) - func(pt2)> tol)
}

result <- gradient.descent(
  ssquares,
  derivative,
  c(0,0),
  .00001,1e-8)


data <- dat.adv



result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the funcion
  c(0,0), # start point of theplot_loss(simple_ex) search
  0.00001, # step size (alpha)
  1e-8) # relative tolerance for one step

print(result) # coordinate of function minimum
print(ssquares(result)) # response of function minimum




#################################################################################################################
#################################################################################################################



data <- my.data

ssquares <- function(x){ # x is beta, vector of coefficients 
  n <- nrow(data) #200
  sum((data[,4] - cbind(1,data[,1])) %*% x)^2 / n
}

derivative <- function(x){ # x is a point
  n <- nrow(data) # 200
  c(sum(-2*(data[,4] = cbind(1, data[,1]) %*% x)), sum(-2*(data[,1])*(data[,4] - cbind(1,data[,1]) %*% x))) /n
}

# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.00001, tol=1e-8) 
{
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol) 
  {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) # print progress
  }
  pt2 # return the last point
}


result <- gradient_descent(
  ssquares, # the function to optimize
  derivative, # the gradient of the funcion
  c(0,0), # start point of theplot_loss(simple_ex) search
  0.01, # step size (alpha)
  1e-8) # relative tolerance for one step

print(result) # coordinate of function minimum
print(ssquares(result)) # response of function minimum






#*************************************************************
#****************** Cross-Validation Example *****************
#*************************************************************
setwd("~/github/Enquist_Mike/Enquist_Mike/AQM Assignments")
# import example Breast Cancer data set
dat.bc <- na.omit(read.csv("BreastCancer.csv", header = TRUE))

# fit model
mod1 <- glm(Class ~ ., dat.bc[,-1], family = "binomial") # remove ID

#------------------ run LOOCV -----------------------------
# initialise prediction vector
isCorrect <- NULL

# perform cv
for(i in 1:nrow(dat.bc))
{
  x.train <- dat.bc[-i,-1] # remove ID
  x.test <- dat.bc[i,-1] # remove ID
  
  mod <- glm(Class ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$Class
  isCorrect[i] <- ifelse(y.pred == y.true, 1, 0)
}

# misclassification error
sum(isCorrect)/length(isCorrect)

#------------------ run K-fold -----------------------------
error <- NULL
nRuns <- 1000
kSize <- 600

# perform k-fold
for(i in 1:nRuns)
{
  randomSample <- sample(1:nrow(dat.bc), kSize, replace = FALSE)
  x.train <- dat.bc[randomSample, -1] # remove ID
  x.test <- dat.bc[-randomSample, -1] # remove ID
  
  mod <- glm(Class ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$Class
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)  
}

# plot error over iterations
plot(cumsum(error)/seq_along(error), type = "l")


#---------------- Bootstrapping ----------------------------
# quantify variance in your model

# fit model
mod <- glm(Class ~ ., dat.bc[,-1], family = "binomial")

# set params
error <- NULL
nRuns <- 50
sampleSize <- 50

for(i in 1:nRuns)
{
  subsetDat <- dplyr::sample_n(dat.bc, sampleSize)
  pred <- predict(mod, newdata = subsetDat[,-1], type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- subsetDat$Class
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)
}

mse <- mean(error^2)


df <- read.csv("BreastCancer.csv")[,-1]
head(df)

#Look up ?factor

df$Class <- factor(df$Class, levels=c(0,1), labels = c("benign","malignant"))
head(df)

fit.logit <- glm(Class~., data=df, family = binomial())
summary(fit.logit)

prob <- predict(fit.logit, df, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), labels = c("benign","malignant"))
#By default predict() function predicts the log odds of having a malignant outcome

logit.perf <- table(df$Class, logit.pred, dnn = c("Actual", "Predicted") )
logit.perf






