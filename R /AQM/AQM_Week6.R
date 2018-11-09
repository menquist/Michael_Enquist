library(quantmod)
library(MASS)
getSymbols("YHOO")
getSymbols("^GSPC")

'get the adjusted close prices of each index'
yahoo.closepriceadj <- as.vector(YHOO$YHOO.Adjusted)
snp.closepriceadj <- as.vector(GSPC$GSPC.Adjusted)

'get the return of the close prices of each (1st difference)'
yahooreturns.vector <- diff(yahoo.closepriceadj)
snpreturns.vector <-diff(snp.closepriceadj)


'a date sequence of each return'
dates.vector <- sequence(length(snpreturns.vector)) 

'construct design matrix'
X = matrix( c(t(rep(1, length(snpreturns.vector))) , t(snpreturns.vector)), 
            + length(snpreturns.vector),
            + 2, 
            + FALSE)

a = solve(t(X)%*%X)%*%t(X)%*%yahooreturns.vector
df <- data.frame(snp = snpreturns.vector, yahoo = yahooreturns.vector)

'plot yahoo returns vs. snp returns and linear regression model'
plot(snpreturns.vector, yahooreturns.vector,col="red")
abline(a[1:2])
hist(snpreturns.vector)
hist(yahooreturns.vector)


'use built in LM function'
lmfit <- lm(yahooreturns.vector ~ snpreturns.vector)
summary(lmfit)
abline(lmfit, col = 'blue')

plot(lmfit)

##### The standard error if the SP_weekly_return is quite high at 0.2932. Additionally, while looking at the R and and the Adjusted R squared, we see that the S&P is a poor indicator of Talisman. The negative R squared should be interpreted as 0 indicating no relation. 

#### We can verify this by doing going through the 4 assumptions 

##### First Assumption: We want the Expected(residuals)=0. 

plot(residuals(lmfit))

###### There are some strong negative residuals. However, overall, there the residuals are centered. We can also plot this using the car package using crPlot(mod) which will use the partial residuals

##### Second Assumption: Constant Variance. 
###### We want the variance of the residuals to be around zero 

plot(fitted(lmfit), residuals(lmfit),xlab="Fitted", ylab="Residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red", "red", "red"), lty = c(2,1,2))

###### According to our graph, there is large variance from zero.

##### Third Assumption: Normality of Errors 

qqnorm(residuals(lmfit) ,ylab="Residuals")
qqline(residuals(lmfit), col="red", lwd = 2)
hist(residuals(lmfit))


##### Fourth Assumption: Uncorrelated Errors 

plot(residuals(lmfit))
acf(residuals(lmfit))
pacf(residuals(lmfit))

crPlots(lmfit)
########################################################################################
##### By running the crplot, we see that potentially a non linear model could fit better. 

model2 <- lm(TLM_weekly_return ~ poly(SP_weekly_return, 3))
anova(model2)
summary(model2)
########################################################################################

'use robust LM'
lmfit <- rlm(yahooreturns.vector ~ snpreturns.vector)
summary(lmfit)
abline(lmfit, col = 'green')

' useful diagnostic functions http://www.statmethods.net/stats/regression.html'

'now lets take a look at the world 3 dataset'
w3 <- read.csv('world3_subset.csv.txt')
head(w3)

explanatory <- w3;
explanatory$X <- NULL;
explanatory$HDI <- NULL;

'build multivariable linear model'
lmfit2 = lm(HDI ~., w3)
summary((lmfit2))
plot(lmfit2)

'Question: how many regression coefficients are significant? Can we remove nonsignificant
variables from the model? Could this affect the significance of other coefficients?'

######################################################## DIANOSTICS
# dependencies
library(plyr)
library(dplyr)
library(car)
library(lmtest)

# load data
dat <- read.csv("World3_subset.csv.txt")[,-1]
head(dat)

# fit model
mod <- lm(HDI ~ ., dat)

# look for collinearity
pairs(dat[,1:5])


#linear relationship
#constant variance
#corelation vs causation
# no autocorrelation
# multi collinearity
#iid, independent distribution, 
# Assumption 1 - Expectation is 0
# If violated, we assume x_j on E(Y) when it is not
plot(cbind(residuals(mod), dat[,5:10]), lower.panel = NULL)

# Component residual plots (partial residual plots)
# Use 'car' package
crPlots(mod)

# Assumption 2 - constant variance
# Common issue is dependence of residuals on E(Y)
plot(fitted(mod), residuals(mod), xlab="Fitted", ylab="Residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red", "red", "red"), lty = c(2,1,2))

plot(fitted(mod), abs(residuals(mod)), xlab="Fitted", main ="|Residuals|")

summary(lm(abs(residuals(mod)) ~ fitted(mod)))

# bptest(p) does the Breuch Pagan test to formally 
# check presence of heteroscedasticity. Low p-value
# indicates higher prob of heteroscedasticity.
bptest(mod)


# Assumption 3 - Normality of errors
# qqplot
qqnorm(residuals(mod), ylab="Residuals")
qqline(residuals(mod), col = "red", lwd = 2)
# histogram
hist(residuals(mod), breaks=50)

# Assumption 4 - Uncorrelated Errors
# Typical in time-series data. If errors are correlated, more advanced methods will
# come into play. Correlation may help us!
plot(residuals(mod))
acf(residuals(mod))
pacf(residuals(mod))

