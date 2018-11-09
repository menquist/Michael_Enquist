# load dependencies
library(quantmod)

# Import data
getSymbols("PD.TO", src = "yahoo")
Natural <- PD.TO

# Check data structure
head(Natural)
PD.TO.tail <- tail(Natural, n = 3650)

# Plot data
lineChart(PD.TO.tail, theme = "black")
