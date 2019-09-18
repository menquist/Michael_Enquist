library(daag)
sqr = seq(1,100)
sqr.squared = NULL
for(n in 1:50)
{sqr.squared[n] = sqr[n]^2}

summary(sqr.squared)
plot(sqr.squared)
str(sqr.squared)
str(sqr)

#FOR i in a sequence along n
#IF i is less than 3 
#DO 
#ElSE n[i] > 3
 

n <- 1:5
r <- NULL
for(i in seq(along= n)){
 if (n[i]<3){r <- c (r,n[i] -1)} else {stop ("Values shall be <3")}}

gpro <- read.csv("gpro.txt", header = F, col.names = c("x1", "x2", "x3", "x4", "y"))
dim(gpro)
summary(gpro)
str(gpro)
head(gpro)
train <- gpro[1:4000,]
test <- gpro[4001:14000,]
x.train <- sample_n(train,200)
x.test <- sample_n(test, 1000)
dim(x.train)
dim(x.test)


Trainx <- train[sample.int(dim(train)[1], size=200), ]
Testx <- test[sample.int(dim(test)[1], size=1000), ]
dim(Trainx)      
dim(Testx)

#cross validation
n <- dim(x.train)[1]
rmse <- rep(0,n)
for(i in 1:n) {
  point <- x.train[i,]
  training <- x.train[-i,0]
  model <- lm(y ~ ., data = training)
  pred <- predict(model, newdata = point)
  rmse[i] <- mean(pred - point$y)^2
}
mean(rmse)


print(rmse)
(Trainx)[1]{ if (Trainx[i]){ loocv <- }}

