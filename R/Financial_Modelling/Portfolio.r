library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(ROI)
# Get data
getSymbols(c("AC.TO", "AEM.TO", "BB.TO", "ARX.TO", "^GSPTSE"))

# Assign to dataframe
# Get adjusted prices merge.zoo(AC.TO[,6], AEM.TO[,6], BB.TO[,6], ARX.TO[,6], ECA.TO[,6])
prices.data <- merge.zoo( AC.TO[,6], AEM.TO[,6], BB.TO[,6], ARX.TO[,6], ECA.TO[,6])
 






############################################################

setwd("~/R_Stuff/Stock_Analysis/TSX.MX-Tickers/")
SP <-  getSymbols(Symbols = "^GSPTSE", src = "yahoo", auto.assign = FALSE )
SP<-data.frame(date=as.character(index(SP)),coredata(SP))
SP.adj <- SP %>% 
  select(date,GSPTSE.Adjusted) %>% 
  mutate(GSPTSE = 100*((GSPTSE.Adjusted - lag(GSPTSE.Adjusted)))/GSPTSE.Adjusted)
SP.adj <- na.omit(SP.adj[-2])

temp = list.files(pattern="*.csv")
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names =T )
  datalist = try(lapply(temp,function(x){read.csv(file=x,header=T,colClasses=c("NULL", NA, NA))}))
  try(Reduce(function(x,y) {merge(x, y, by="date", all.x=TRUE)}, datalist))
}

mydata <- multmerge("")
valid_column_names <- make.names(names=names(mydata), unique=TRUE, allow_ = TRUE)
names(mydata) <- valid_column_names
#### choose tickers!!!!
#date <- mydata$date
#mydata.1  <- mydata
#mydata <- cbind(date,mydata.1)
#######################
combined <- sort(union(levels(SP.adj$date), levels(mydata$date)))
n <- left_join(mutate(SP.adj, a=factor(date, levels=combined)),
               mutate(mydata, a=factor(date, levels=combined)))
n <- n[-3]
mydata <- na.omit(n)



# Calculate returns
#returns.data <- CalculateReturns(prices.data)
#returns.data <- na.omit(returns.data) 
tsx.stock <- mydata[c(1,7,22,39,49,55,66,83)]
tsx.stock <- na.omit(tsx.stock)

ts.date <- tsx.stock
ts.date <- ts.date
tsx.ticker <- tsx.stock[2:12]
tsx.stock <-  apply(tsx.ticker,2,function(x) diff(log(x)))
tsx.stock <- rbind(ts.date,tsx.stock)

tsx.stock <- na.omit(mydata[1:10])



qxts <- xts(tsx.stock[,-1], order.by=as.Date(tsx.stock$date))
returns.data <- merge.zoo(qxts)
returns.data <- na.omit(returns.data) 
# Set names
#colnames(returns.data) <- c("MSFT", "SBUX", "IBM", "AAPL", "^GSPC", "AMZN")

# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)
#Now that we have some data, let’s get started by creating a portfolio specification. This can be done by using portfolio.spec()

# Start with the names of the assets
port <- portfolio.spec(assets = colnames(returns.data))
#Now for some constraints. Let’s use the following:
 # Box constraints
#Leverage (weight sum)
# Box

# Bx
#port <- add.constraint(port, type = "box",
#port <- add.constraint(port, "weight_sum", min_sum=0.99, max_sum=1.01)
port <- add.constraint(port, "box", min=0, max=0.99)
# Leverage
port <- add.constraint(portfolio = port, type = "full_investment" )
# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 10000, rp_method = "sample")

#Now let’s add some objectives and optimize. For, simplicity’s sake let’s do some mean-variance optimization.

# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")

# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")

# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)

# Generate vector of returns
minret <- 0.06/100
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)
#Now that we have the minimum variance as well as the maximum return portfolios, we can build out the efficient frontier. Let’s add a weight concentration objective as well to ensure we don’t get highly concentrated portfolios.

#Note:random_portfolios() ignores any diversification constraints. Hence, we didn’t add it previously.
#Using the random solver for each portfolio in the loop below would be very compute intensive. We’ll use the ROI (R Optmization Infrastructure) solver instead.
eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
  # eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
  #                            conc_aversion = 0.001)
  
  eff.port <- optimize.portfolio(returns.data, eff.port, optimize_method = "ROI")
  
  eff.frontier$Risk[i] <- sqrt(t(eff.port$weights) %*% covMat %*% eff.port$weights)
  
  eff.frontier$Return[i] <- eff.port$weights %*% meanReturns
  
  eff.frontier$Sharperatio[i] <- eff.port$Return[i] / eff.port$Risk[i]
  
  frontier.weights[i,] = eff.port$weights
  
  print(paste(round(i/length(vec) * 100, 0), "% done..."))
}
#Now lets plot !
  
  feasible.sd <- apply(rportfolios, 1, function(x){
    return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
  })

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd

p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(eff.frontier, x = Risk, y = Return, mode = "markers", 
            type = "scattergl", showlegend = F, 
            marker = list(color = "#F7C873", size = 5)) %>% 
  
  layout(title = "Random Portfolios with Plotly",
         yaxis = list(title = "Mean Returns", tickformat = ".2%"),
         xaxis = list(title = "Standard Deviation", tickformat = ".2%"),
         plot_bgcolor = "#434343",
         paper_bgcolor = "#F8F8F8",
         annotations = list(
           list(x = 0.4, y = 0.75, 
                ax = -30, ay = -30, 
                text = "Efficient frontier", 
                font = list(color = "#F6E7C1", size = 15),
                arrowcolor = "white")
         ))



frontier.weights.melt <- reshape2::melt(frontier.weights)

q <- plot_ly(data.frame(frontier.weights.melt), x=Var1, y = value, group = Var2, type = "bar") %>%
  layout(title = "Portfolio weights across frontier", barmode = "stack",
         xaxis = list(title = "Index"),
         yaxis = list(title = "Weights(%)", tickformat = ".0%"))

plot_ly(data = data.frame(frontier.weights.melt))
###################################################################################################################



R <- returns.data
funds <- colnames(R)

# Set up an initial portfolio object with basic constraints
init.portf <- portfolio.spec(assets=colMeans(returns.data))

# Add an objective to maximize mean return per unit expected shortfall
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")

# dollar neutral portfolio
dollar.neutral.portf <- init.portf
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="weight_sum", 
                                       min_sum=-0.01, max_sum=0.01)
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="box", min=-0.5, max=0.5)

# Here is a dollar neutral portfolio with no constraint on leverage
opt1 <- optimize.portfolio(R = returns.data, portfolio=dollar.neutral.portf, 
                           optimize_method="DEoptim", search_size=2000, 
                           trace=TRUE)
sum(opt1$weights)

# Total portfolio leverage is actually greater than 4
sum(abs(opt1$weights))

# now add the leverage exposure constraint for 2:1 leverage
dollar.neutral.portf <- add.constraint(portfolio=dollar.neutral.portf, 
                                       type="leverage_exposure", leverage=2)
# Run optimization
opt2 <- optimize.portfolio(R=R, portfolio=dollar.neutral.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt2$weights)
sum(abs(opt2$weights))

# Leveraged portfolio
leveraged.portf <- init.portf

# Add a "leverage" constraint using max_sum
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="leverage", 
                                  min_sum=1.29, max_sum=1.31)
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="box", min=-0.3, max=0.6)

opt3 <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt3$weights)

# total portfolio leverage is approximately 3.9
sum(abs(opt3$weights))

# add a leverage exposure constraint
leveraged.portf <- add.constraint(portfolio=leveraged.portf, 
                                  type="leverage_exposure", leverage=1.5)

# change min_sum and max_sum such that the weights sum to 1
leveraged.portf$constraints[[1]]$min_sum <- 0.99
leveraged.portf$constraints[[1]]$max_sum <- 1.01

# Run optimization
opt4 <- optimize.portfolio(R=R, portfolio=leveraged.portf, 
                           optimize_method="DEoptim",
                           search_size=2000)
sum(opt4$weights)
# total portfolio leverage is less than 1.5
sum(abs(opt4$weights))

###########################################
R <- returns.data
funds <- colnames(R)

# create an xts object of regimes
# Here I just randomly samples values to create regime 1 or regime 2. In 
# practice, this could based on volatility of other regime switching models
set.seed(123)
regime <- xts(sample(1:2, nrow(R), replace=TRUE), index(R))

# portfolio for regime 1
port1 <- portfolio.spec(funds)
port1 <- add.constraint(port1, "weight_sum", min_sum=0.99, max_sum=1.01)
port1 <- add.constraint(port1, "box", min=0, max=0.99)
port1 <- add.objective(port1, type="risk", name="ES", arguments=list(p=0.9))

# portfolio for regime 2
port2 <- portfolio.spec(funds)
port2 <- add.constraint(port2, "weight_sum", min_sum=0.99, max_sum=1.01)
port2 <- add.constraint(port2, "box", min=0.01, max=0.7)
port2 <- add.objective(port2, type="return", name="mean")
port2 <- add.objective(port2, type="risk", name="ES", arguments=list(p=0.9))

portfolios <- combine.portfolios(list(port1, port2))

regime.port <- regime.portfolios(regime, portfolios)

# should result in portfolio for regime 1
optimize.portfolio(R, regime.port, optimize_method="random", search_size=10000, trace=TRUE)

# should result in portfolio for regime 2
optimize.portfolio(R[1:(nrow(R)-2)], regime.port, optimize_method="random", search_size=10000, trace=TRUE)

returns <- returns.data
funds <- colnames(returns)
init.portfolio <- portfolio.spec(assets = funds)
print.default(init.portfolio)
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment")
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only")
# Add objective for portfolio to minimize portfolio standard deviation
minSD.portfolio <- add.objective(portfolio=init.portfolio, 
                                 type="risk", 
                                 name="StdDev")

# Add objectives for portfolio to maximize mean per unit ES
meanES.portfolio <- add.objective(portfolio=init.portfolio, 
                                  type="return", 
                                  name="mean")

meanES.portfolio <- add.objective(portfolio=meanES.portfolio, 
                                  type="risk", 
                                  name="ES")
print(minSD.portfolio)
print(meanES.portfolio)
# Run the optimization for the minimum standard deviation portfolio
minSD.opt <- optimize.portfolio(R = returns, portfolio = minSD.portfolio, 
                                optimize_method = "ROI", trace = TRUE)

print(minSD.opt)
# Run the optimization for the maximize mean per unit ES
meanES.opt <- optimize.portfolio(R = returns, portfolio = meanES.portfolio, 
                                 optimize_method = "ROI", trace = TRUE)

print(meanES.opt)

plot(minSD.opt, risk.col="StdDev", chart.assets=TRUE, 
     main="Min SD Optimization",
     ylim=c(0, 0.0083), xlim=c(0, 0.06))

plot(meanES.opt, chart.assets=TRUE, 
     main="Mean ES Optimization",
     ylim=c(0, 0.0083), xlim=c(0, 0.16))

