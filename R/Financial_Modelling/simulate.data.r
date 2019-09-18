library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")

# Plug year in
#-----------------------------------------
year <- "1900-01-01" # <== plug in here
#-----------------------------------------



CAD <- Quandl("FRED/CSHCCPCAA156NRUG", start_date=year, collapse="daily")


a <- c(2500,3600)
s <- mean(CAD$VALUE)
x <- sd(CAD$VALUE)
s
x

#example data
df <- data.frame(country=c("a" ), 
                 mean=c(s), 
                 sd=c(x))

n <- 2000

#ncol = F
#function which returns a matrix, and takes column vectors as arguments for mean and sd
normv <- function( n , mean , sd ){
  out <- rnorm( n*length(mean) , mean = mean , sd = sd )
  return( matrix( out ,nrow = n , byrow = T) )
}

#reproducible result (note order of magnitude of rows and input sample data)
set.seed(1)
t <- normv( n , df$mean , df$sd )
x
s
t

