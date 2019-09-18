library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)

# Get data
#getSymbols(c("BMO.TO", "G.TO", "AEM.TO", "TRP.TO", "CFP.TO", "RY.TO", "WFT.TO", "LUN.TO","FM.TO"))

symbols <- c("BMO.TO","RY.TO","LUN.TO","FM.TO","QSR.TO","FTT.TO","T.TO","IFC.TO","SAP.TO","SNC.TO","SU.TO","L.TO","GIB-A.TO")

year <- "1900-01-01" # <== plug in here
#data.env <- new.env()
dataset <- xts()
# cool progress bar to see the % of completion
n <- length(symbols)
pb <- txtProgressBar(min = 0, max = n, style=3)


# Actual loop: 
for(i in 1:length(symbols)) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from=year, src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from=year, src='yahoo')
    dataset <- merge(dataset, Ad(get(symbols[i])))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}


str(dataset)

#dat  <- data.frame(date=index(dataset), coredata(dataset))
#index(dataset) <- round(index(dataset),"day")


rm(list=setdiff(ls(), c("dataset", "perform_vec","symbols","year")))



dat1 <- as.data.frame(dataset)
nr <- nrow(dat1)
dat1 <- (dat1[-1,] - dat1[-nr,]) / dat1[-nr,]



library(functional)

do.call(data.frame,lapply(dat1, function(x) replace(x, is.infinite(x),NA)))




dat  <- data.frame(date=round(index(dataset[-1,],"day")), coredata(dat1))
dat <- cbind(ticker = rownames(dat), dat)
rownames(dat) <- 1:nrow(dat)
date  <- dat[,2]
dat <- dat[,-c(1)]

date <- round(date,"days")


dat <- dat[,colSums(is.na(dat))<nrow(dat)]

str(dat)

stockname <- colnames(dat)

#stockname <-gsub("X", "", stockname)
#data <- dat[100:200]

stockname <-gsub(".Adjusted", "", stockname)

returns.data <- dat1
returns.data <- na.omit(returns.data)
colnames(returns.data) <- symbols
# Assign to dataframe
# Get adjusted prices
#prices.data <- merge.zoo(MSFT[,6], SBUX[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])


#returns.data <- dat
#returns.data.1 <- returns.data
#returns.data.1 <- na.omit(returns.data.1)

# Calculate returns
#returns.data <- CalculateReturns(prices.data)
#returns.data <- na.omit(returns.data)
#eturns.data <- returns.data[,-c(1)]



# Set names
#colnames(returns.data) <- stockname
#
# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)  
covMat <- cov(returns.data)
#Now that we have some data, let’s get started by creating a portfolio specification. This can be done by using portfolio.spec()

# Start with the names of the assets
port <- portfolio.spec(assets = symbols)
#Now for some constraints. Let’s use the following:
  
#  Box constraints
#Leverage (weight sum)
# Box
port <- add.constraint(port, type = "box", min = 0.01, max = 0.6)

# Leverage
port <- add.constraint(portfolio = port, type = "full_investment")
#Let’s use the built-in random solver. This essentially creates a set of feasible portfolios that satisfy all the constraints we have specified. For a full list of supported constraints see here

#port <- add.objective(portfolio=port, type="risk", name="CVaR",
#                      arguments=list(p=0.95, clean="boudt")) 

# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 100000, rp_method = "sample")
#Now let’s add some objectives and optimize. For simplicity’s sake let’s do some mean-variance optimization.

# Add objective to minimize CVaR
#port <- add.objective(portfolio=port, type="risk", name="CVaR",
#                         arguments=list(p=0.95, clean="boudt")) # Add objective for an upper 40% CVaR allocation
#port <- add.objective(portfolio=port, type="risk_budget_objective", name="CVaR", max_prisk=0.4,
#                         arguments=list(p=0.95, clean="boudt"))

# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")


# Optimize
minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", 
                                 rp = rportfolios)
minvar.opt
# Generate maximum return portfolio
maxret.port <- add.objective(port, type = "return", name = "mean")
maxret.port
# Optimize
maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", 
                                 rp = rportfolios)
maxret.opt
# Generate vector of returns
minret <- 0.01/100
maxret <- maxret.opt$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)

eff.frontier <- data.frame(Risk = rep(NA, length(vec)),
                           Return = rep(NA, length(vec)), 
                           SharpeRatio = rep(NA, length(vec)))

frontier.weights <- mat.or.vec(nr = length(vec), nc = ncol(returns.data))
colnames(frontier.weights) <- colnames(returns.data)

library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


for(i in 1:length(vec)){
  eff.port <- add.constraint(port, type = "return", name = "mean", return_target = vec[i])
  eff.port <- add.objective(eff.port, type = "risk", name = "var")
   eff.port <- add.objective(eff.port, type = "weight_concentration", name = "HHI",
                             conc_aversion = 0.001)
  
  
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

#plot(prt.ef$frontier[,c(2,1)],col=2)

p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(data = eff.frontier, x = "Risk" , y = "Return", mode = "markers", 
            type = "scattergl", showlegend  = F, 
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
p
minvar.opt
maxret.opt
eff.port
library(beepr)
beep(4)