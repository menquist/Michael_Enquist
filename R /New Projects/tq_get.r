#' # Load libraries
 library(tidyquant)
 library(Quandl)
 library(alphavantager)
 
#'
#' # Get the list of `get` options
tq_get_options()
#'
#' # Get stock prices for a stock from Yahoo
 aapl_stock_prices <- tq_get("AAPL")
#'
#' # Get stock prices for multiple stocks
 mult_stocks <- tq_get(c("FB", "AMZN"),
                       get  = "stock.prices",
                       from = "2016-01-01",
                       to   = "2017-01-01")
#'
#' # Multiple gets
 mult_gets <- tq_get("AAPL",
                     get = c("stock.prices", "financials"),
                     from = "2016-01-01",
                     to   = "2017-01-01")
str(mult_gets)
 
a <- tq_get("MSFT",get = c("key.ratios"))
b <- tq_get(c("FB", "AMZN"),get = c("key.ratios"),
            from = "2016-01-01",
            to   = "2017-01-01")
d <- b %>% unnest(data) 

# ZLX1IQYC2DN82YK7
c <- tq_get("MSFT",get = c("alphavantager"), av_fun = "TIME_SERIES_INTRADAY")
args(av_get)

e <- av_get(av_fun = "SECTOR")
str(a)
d <- expand(a)
 
d <- a[2]

df = as.data.frame(a)
library(tidyr)
d <- a %>% unnest(data) 

write.csv(a,"data_output/MSFT-key.ratios.csv")
e <- av_get("AAPL" )

validate_get("AAPL")

e <- av_get(symbol = "MSFT", av_fun = "TIME_SERIES_INTRADAY", interval = "15min")


f <- mult_gets %>% unnest(financials)
f <- f %>% unnest(quarter)
