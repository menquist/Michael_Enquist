seed <- 1152 
n <- 274     #nb of observations
a <- 5.932662625       #intercept
b <- .180220089     #slope

set.seed(seed)
epsilon <- rnorm(n, mean=0, sd=sqrt(0.25))
x <- sample(x=c(0, 1), size=n, replace=TRUE)
y <- a + b * x + epsilon
#-----------------------------------------------------------

#------using lm------
mod <- lm(y ~ x)
#--------------------

#------using the explicit formulas------
X <- cbind(1, x)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
var_betaHat <- anova(mod)[[3]][2] * solve(t(X) %*% X)
#---------------------------------------

#------comparison------
#estimate
mod$coef


c(betaHat[1], betaHat[2])


#standard error
summary(mod)$coefficients[, 2]


sqrt(diag(var_betaHat))

num <- n * anova(mod)[[3]][2]
denom <- n * sum(x^2) - sum(x)^2
 sqrt(num / denom)
 
 