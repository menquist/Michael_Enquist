#### Import data for Talisman Energy Inc.

options("getSymbols.warning4.0"=FALSE)

TLM <- getSymbols(Symbols = "YHOO", src = "yahoo", from = "2013-01-01", auto.assign = FALSE)

#### Import S&P 500 data

SP <- getSymbols(Symbols = "^GSPC", src = "yahoo", from = "2013-01-01", auto.assign = FALSE)

### Examining data for Talisman Energy 

#### str() allows us to examine the internal structure 

str(TLM)

#### Dim shows us the dimensions of TLM which in this case are 258 rows and 6 columns

dim(TLM)

#### head shows us the values of the first 5 trading days of TLM within the time frame set


head(TLM)

#### A vector of days that are in our data is shown through time()

time(TLM)

### Now we can find the same information for the S&P 500 Market Index

#### Internal structure of SP

str(SP)

###### This shows us details such the dimensions, and column names.

#### Dimensions are 259 rows and 6 columns

dim(SP)

#### The first 5 days of our time series

head(SP)

#### Vector of all the trading days included in our range 

time(SP)

### The return can be calcated through different methods 

retTLM1 <- diff(TLM$TLM.TO.Adjusted)/(TLM$TLM.TO.Adjusted[-length(TLM$TLM.TO.Adjusted)])

head(retTLM1)
tail(retTLM1)

retTLM <- periodReturn(TLM$TLM.TO.Adjusted)
retSP <- periodReturn(SP$GSPC.Adjusted)

#### Using allReturns function provides us with a matrix showing all the different period returns. 

allReturns(TLM)

#### It seems that daily is the most reliable data since it has the most detail and shows all small changes.

dailyret_TLM <- dailyReturn(TLM$TLM.TO.Adjusted)
dailyret_SP <- dailyReturn(SP$GSPC.Adjusted)

##### The daily returns have different dimensions

dim(dailyret_TLM)
dim(dailyret_SP)

#### Here is the weekly data

weeklyret_TLM <- weeklyReturn(TLM$TLM.TO.Adjusted)
weeklyret_SP <- weeklyReturn(SP$GSPC.Adjusted)

#### It has the same dimensions and therefore, we will use it for our graph. 

dim(weeklyret_TLM)
dim(weeklyret_SP)

#### Transpose.

TLM_weekly_return <- as.vector(weeklyret_TLM)
SP_weekly_return  <- as.vector(weeklyret_SP)

#### We can now plot it. 

plot(SP_weekly_return, TLM_weekly_return, type ='p')

#### Run the regression

model <- lm(TLM_weekly_return ~ SP_weekly_return)
summary(model)
anova(model)

mod <- lm(TLM_weekly_return~SP_weekly_return)
```

##### The standard error if the SP_weekly_return is quite high at 0.2932. Additionally, while looking at the R and and the Adjusted R squared, we see that the S&P is a poor indicator of Talisman. The negative R squared should be interpreted as 0 indicating no relation. 

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

##### By running the crplot, we see that potentially a non linear model could fit better. 

model2 <- lm(TLM_weekly_return ~ poly(SP_weekly_return, 3))
anova(model2)
summary(model2)

