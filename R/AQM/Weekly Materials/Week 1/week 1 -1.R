Hubble <- read.table("Hubble.txt", header = T)

plot(Hubble$x,Hubble$y, type="l")

# Looking at Hubble and portfolio 
plot(Hubble$x, Hubble$y)

ret <- read.table("Portfolio_Return.txt", header = T)


plot(ret$portfolio_ret, ret$stock_ret)
# Run a regression
model <- lm(stock_ret ~ portfolio_ret , data = ret)
summary(model)

plot(portfolio$portfolio_ret,portfolio$stock_ret, type="l")

