# load dependencies
library(quantmod)

# Import data
getSymbols("NGZ14", src = "yahoo")
Patteron <- PTEN

# Check data structure
head(Patteron)
PTEN.tail <- tail(Patteron, n = 3650)

# Plot data
lineChart(PTEN.tail, theme = "black")
