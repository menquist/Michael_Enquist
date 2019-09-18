library(quantmod)
library(plyr)
library(dplyr)

getSymbols("^HSI", src = "yahoo", from = "1988-01-02", to= "1998-09-30", auto.assign = FALSE)
Hongkong <- HSI


getSymbols("^N225", src = "yahoo", from = "1988-01-02", to= "1998-09-30", auto.assign = FALSE)
Nikkei <- N225

getSymbols("^GSPC", src = "yahoo", from = "1988-01-02", to= "1998-09-30", auto.assign = FALSE)
SP <- GSPC

getSymbols("^FTSE", src = "yahoo", from = "1988-01-02", to= "1998-09-30", auto.assign = FALSE)
London <- FTSE

HK <- Hongkong[,6]
NK <- Nikkei[,6]
Sp <- SP[,6]
UK <- London[,6]

dim(HK)
dim(NK)
dim(Sp)
dim(UK)

H <- data.frame(date = as.factor(time(HK)), HK = as.numeric(HK))
N <- data.frame(date = as.factor(time(NK)), HK = as.numeric(NK))
S <- data.frame(date = as.factor(time(Sp)), HK = as.numeric(Sp))
U <- data.frame(date = as.factor(time(UK)), HK = as.numeric(UK))
date
dim(H)
dim(N)
dim(S)
dim(U)

x1 <- inner_join(H, N, by = "date")
x2 <- inner_join(x1, S, by = "date")

dim(x1)
dim(x2)

data <- inner_join(x2, U, by = "date") %>%
  mutate(H = (H - lag(H))/H,
         N = (N - lag(N))/N,
         S = (S - lag(S))/S,
         U = (U- lag(U))/U) %>%

fliter (date != "1988-01-04") %>%
select(-date)

head(data)

summary(data)

dim(x1)
dim(x2)
dim(data)





Greek <- gapminder %>%
  mutate(Ratepop = 100*((pop - lag(pop)))/pop) %>%
  filter(year, country == "Greece") %>%
  filter(year > 1952) 
p3 <-  ggplot(Greek, aes(x=year, y=Ratepop, colour=country )) + geom_line() + geom_point() +  geom_line(colour = "blue") + ggtitle("Greece Change in Population" )
p3