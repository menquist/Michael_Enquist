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
x <- 1:10
y <- c(rep(0, 4),1,0,1,0,1,1)
a <- logistic(x,y,1,10,0.01,0.000000001)
#calculatell(y,findpi(x_new,a))
#Log Likelihood = 0.01343191
data <- cbind(x,y)
data <- as.data.frame(data)
mylogit <- glm(y ~ x, data = data, family = "binomial")
mylogit
preds <- predict(mylogit, newdata = data,type ="response")
calculatell(data$y,preds)
#Log Likelihood = 0.01343191
#Isn't this amazing!!!


