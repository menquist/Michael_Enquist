library(quantmod)

getSymbols("KMI", src = "yahoo", from = "2014-01-02")
Kinder <- KMI
lineChart(Kinder, theme = "white")

getSymbols("^GSPC", src = "yahoo", from = "2014-01-02")
SP <- GSPC
lineChart(SP, theme = "white")

str(Kinder)
dim(Kinder)
head(Kinder)
time(Kinder)
StockAdj <- KMI$KMI.Adjusted
SPAdj <- GSPC$GSPC.Adjusted
Stock <- diff(StockAdj)/StockAdj[-length(StockAdj)][-1,]
Stockreturn <- weeklyReturn(StockAdj)
dim(Stock)

SPRet <- diff(SPAdj)/SPAdj[-length(SPAdj)][-1,]
SPreturn <- weeklyReturn(SPAdj)

dim(Stock)
dim(SPRet)

# Run a regression
model <- lm(Stock ~ SPRet)
summary(model)

`plot(ts(Stock), ts(SPRet), type = "p")

anova(model)
