setwd("~/Downloads")

f1 <- function(pal){
  x<- read.csv(pal)
  x<- as.data.frame(x)
  return(x)
}





Workbrain.BS <- f1("Workbrain Balance sheet.csv")
Workbrain.CF <- f1("workbrain cashflows.csv")
Workbrain.IS <- f1("WorkBrain Income Statement.csv")

xts(Workbrain.BS[, -1], order.by=as.POSIXct(Workbrain.BS$X))
qxts <- xts(Workbrain.BS[,-1], order.by=q[,1])
### Melting a
Workbrain.BS <- melt(Workbrain.BS)
Workbrain.BS
### Metling b
b1 <- melt(IS)
b1
### Melting C
c1 <- melt(CF)
c1

##### VAR1 ANd VAr2
names(a2) <- c("X1", "X2", "value")
names(b1) <- c("X1", "X2", "value")  
names(c1) <- c("X1", "X2", "value")

########################################################################### ROIC 


### average net debt
net_debt <- filter(Workbrain.BS, X %in% c("Cash and Equivalents", "Total Current Liabilities",  "Total Shareholders\xd5 Equity"))

Net_debt <- net_debt %>% 
  select(X,X2002,X2003) %>% 
  
Net_debt
NET_Debt <- Net_debt$NB
ave_net_debt <- mean(NET_Debt)
equity <- net_debt$Equity

##ebit
ebit_value <- EBIT$optincome 



#### ROIC
ROIC <- ebit_value/(NET_Debt+equity)
ROIC

# Extract Inputs for DCF Valuation
#******************************************************************                 
# Free Cash Flows
FCF = get.fund.data('free cash flow', fund, fund.date)

# Invested Capital
IC = get.fund.data('invested capital', fund, fund.date)

# Sales
SALE = get.fund.data('total revenue', fund, fund.date)

# Common Equity
CEQ = get.fund.data('total equity', fund, fund.date)

# Common Shares Outstanding
CSHO = get.fund.data('total common shares out', fund, fund.date)

# Growth Rate
CROIC = FCF/IC

# Average inputs
g = runMean(CROIC, 5)
cash = runMean(FCF, 5)
#Next I created a simple function to estimate company’s Intrinsic Value using Discounted Cash Flow (DCF) analysis.

#*****************************************************************
# Helper function to compute Intrinsic Value
#******************************************************************                 
compute.DCF.IV <- function(cash, eqity, shares, g, R) {
  if( cash <= 0 ) return(NA)
  
  if( len(R) == 1 ) R = rep(R, len(g))
  
  value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
  return( value / shares )
}
#Finally, let’s compute AAPL’s Intrinsic Value and create plots

#*****************************************************************
# Compute Intrinsic Value, assumptions:
# Company will grow for the first 3 years at current Growth Rate
# slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3 years
# and finally 3% growth for the next 10 years
#
# The Discount Rate is 9%
#
# http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
#******************************************************************                 
dcf.price = NA * g
i.start = which(!is.na(g))[1] 

for(i in i.start : nrow(g)) {
  # Create Growth Rate scenario:      
  g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
  
  # Compute Intrinsic Value
  dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 9/100)
}

#*****************************************************************
# Create Plots
#****************************************************************** 
plota(price, type='l', log = 'y', col='blue', main=tickers[1],
      ylim=range(price,dcf.price,na.rm=T))
plota.lines(dcf.price, type='s', col='red', lwd=2)
plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))   


plota(g, type='b', col='blue', pch=0, main='Growth Rate')   


plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')    



