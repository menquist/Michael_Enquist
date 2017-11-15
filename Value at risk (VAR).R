#Value at Risk (VaR) - A Stochastic Simulation Approach
library(quantmod) # to load stock data
library(MASS) # for a multivariate random normal generator
library(reshape2) # to melt dframes into long form
library(ggplot2)
library(GGally) # some nice plots not available in ggplot

# load a portfolio of stocks using getSymbols from quantmod
tickers <- c("RY.TO", "TD.TO", "", "", "")
quantmod::getSymbols(tickers, from = "2013-01-01")
stockPrices <- lapply(tickers, function(x) eval(parse(text=x))[,6]) %>%
  as.data.frame() %>% setNames(tickers)

# plot stock prices
plot.stocks <- stockPrices %>%
  melt(variable.name="tickers", value.name="close.price") %>%
  data.frame(index = rep(1:nrow(stockPrices), length(tickers))) %>%
  ggplot(aes(x=index, y=close.price, colour = tickers, group=tickers)) +
  geom_line() +
  facet_wrap(~tickers, scale="free") +
  theme_bw()
print(plot.stocks)

# compute matrix of returns
returnsMatrix <- stockPrices %>%
  apply(2, function(x) diff(log(x)))

# plot differenced returns
data.frame(index = 1:nrow(returnsMatrix), returnsMatrix) %>% 
  melt(id.vars = "index", variable.name = "ticker", value.name = "return") %>%
  ggplot(aes(x=index, y=return, colour=ticker, group=ticker)) +
  geom_line() + facet_wrap(~ticker) + theme_bw()

# compute the covariance matrix - use this covatiance matrix
# to simulate multivariate normal random variables. This will
# ensure that the correlation structure remains intact
covMatrix <- cov(returnsMatrix)

# vector of mean values
meanVector <- apply(returnsMatrix, 2, mean)

# plot the data pairwise to identify correlations
GGally::ggpairs(returnsMatrix, colour=1) + theme_bw()


##################################################################

# Now it's time to simulate the prices based utilising the
# correlation structure through a multivariate normal.
# N simulations are performed for the entire matrix, meaning
# that each stock will have N simulations.

# get the mean price
S0 <- apply(stockPrices, 2, mean)

# initialise lists - why am I initialising lists?
simulatePrice <- list()
N.iterations <- list()

# change me
#-----------------------------
N <- 1000 # number of simulated runs for all stocks
t <- 60 # number of days into the future to simulate
#-----------------------------

# iterate over number of simulations
for(i in 1:N) {
  
  # for each simulation of the portfolio, regenerate a 
  # number of multivariate normal draws with a mean 
  # vector and covariance matrix
  randomMatrix <- mvrnorm(n = t, 
                          mu = meanVector, Sigma = covMatrix)
  
  # for each of the N iterations, simulate one sequence of each
  # stock - one future possibility
  for(j in 1:ncol(returnsMatrix)){
    
    # useing a geometric brownian motion simulation (a sequence of
    # log normal distributions) - each time step will be a jump
    # drawn from a lognormal distribution
    simulatePrice[[j]] <- round(S0[j]*exp(cumsum(randomMatrix[,j])), 2)
  }
  
  # transform the simulatedPrice sequence into a clean, logical format
  newIteration <- data.frame(iteration=i, t(ldply(simulatePrice))) %>% 
    setNames(c("iteration",tickers))
  
  # input the transformed simulatedPrice data.frame into the ith 
  # element of the N.iterations list, then start the outer loop 
  # again.
  N.iterations[[i]] <- newIteration
}


# map the list into a data.frame and change iteration
# column to a factor
price.sim <- ldply(N.iterations) %>% 
  mutate(iteration = as.factor(iteration))

# plot a sample of 20 iterations for each stock price
plot.sim <- price.sim %>% 
  melt(id.vars = "iteration", value.name = "return", variable.name = "ticker") %>%
  data.frame(index = rep(1:t)) %>%
  mutate(iteration = as.integer(iteration)) %>%
  filter(iteration <= 100) %>%
  mutate(iteration = as.factor(iteration)) %>%
  group_by(ticker, iteration) %>%
  ggplot(aes(x=index, y=return, colour=iteration)) +
  geom_line(size=0.5, show_guide = FALSE) + facet_wrap(~ticker, scale="free") +
  theme_bw()
print(plot.sim)
####################################################################################
# calculate the expected price at the final time T of the simulations
expected.price <- data.frame(index = rep(1:t, N), price.sim) %>%
  filter(index == max(unique(index))) %>%
  dplyr::select(-c(index, iteration)) %>%
  apply(2, mean)

# calculate the covariance matrix at the final time T of the simulations
covariance.price <- data.frame(index = rep(1:t, N), price.sim) %>%
  filter(index == max(unique(index))) %>%
  dplyr::select(-c(index, iteration)) %>%
  cov()

# number of assets
nAssets <- length(tickers) 

# set up constraints and optimise
aMat <- array(1, dim = c(1,nAssets))
bVec <- 1
zeros <- array(0, dim = c(nAssets,1))
QPoptim <- quadprog::solve.QP(covMatrix, zeros, t(aMat), bVec, meq = 1)

# visualise weights
ggplot(data.frame(weights=QPoptim$solution, ticker=as.factor(tickers))) +
  geom_bar(stat="identity", aes(x=ticker, y=weights), fill = "skyblue") +
  theme_bw()

# specify portfolio weight vector
weights <- QPoptim$solution

# Expected price on portfolio
mu.portfolio <- t(weights) %*% expected.price

# portfolio volatility 
sigma.portfolio <- t(weights) %*% covariance.price %*% weights

# VaR - set initial investment
#--------------------
investment <- 1000000
#--------------------

# calculate VaR by multiplying the sdev of the portfolio by the
# investment, then the quantile representing a tail alpha of 0.05.
VaR <- investment * sqrt(sigma.portfolio) * qnorm(p = 0.95)

# plot distribution
curve(dnorm(x, investment*mu.portfolio, investment*sqrt(sigma.portfolio)), 
      xlim=c(-3e6, 2e7), ylab="density", xlab="portfolio ret.", col="blue",
      lwd=2)
abline(v=mu.portfolio*investment, lty=2); abline(v = VaR, col="red", lwd=2)

