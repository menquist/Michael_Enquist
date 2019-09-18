
library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(plotly)
library(doMC)
# Get data
getSymbols(c("MSFT", "GOOG", "IBM", "AAPL", "^GSPC", "AMZN"))

# Assign to dataframe
# Get adjusted prices
prices.data <- merge.zoo(MSFT[,6], GOOG[,6], IBM[,6], AAPL[,6], GSPC[,6], AMZN[,6])

# Calculate returns
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)

# Set names
colnames(returns.data) <- c("MSFT", "GOOG", "IBM", "AAPL", "^GSPC", "AMZN")

# Save mean return vector and sample covariance matrix
meanReturns <- colMeans(returns.data)
covMat <- cov(returns.data)


# Start with the names of the assets
port <- portfolio.spec(assets = c("MSFT", "GOOG", "IBM", "AAPL", "^GSPC", "AMZN"))

# Box
port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)

# Leverage
port <- add.constraint(portfolio = port, type = "full_investment")
# Generate random portfolios
rportfolios <- random_portfolios(port, permutations = 50000, rp_method = "sample")

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



feasible.sd <- apply(rportfolios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% covMat %*% matrix(x, ncol = 1)))
})

feasible.means <- apply(rportfolios, 1, function(x){
  return(x %*% meanReturns)
})

feasible.sr <- feasible.means / feasible.sd





install.packages("ploty")

#PLOTY

p <- plot_ly(x = feasible.sd, y = feasible.means, color = feasible.sr, 
             mode = "markers", type = "scattergl", showlegend = F,
             
             marker = list(size = 3, opacity = 0.5, 
                           colorbar = list(title = "Sharpe Ratio"))) %>% 
  
  add_trace(data = eff.frontier, x = Risk, y = Return, mode = "markers", 
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





