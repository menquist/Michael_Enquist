# World Bank API
library(WDI)

# retrieve GDP from World Bank api
gdp1 <- WDI(country="all", indicator = "NY.GDP.PCAP.CD") %>% na.omit()
colnames(gdp) <- c("iso2c", "country", "gdpPercap", "year") 
View(gdp1)

gdp <- gdp1 %>% 
  filter(year == "2005")

# derive mean and sd
mean.logGDP <- mean(log(gdp$NY.GDP.PCAP.CD))
sd.logGDP <- sd(log(gdp$NY.GDP.PCAP.CD))

# plot histogram
ggplot(gdp, geom = "density", aes(NY.GDP.PCAP.CD)) + 
  geom_histogram(stat = "bin", binwidth=500, aes(y=..density..)) +
  geom_density(colour="red", size = 1) + 
  stat_function(fun = dlnorm, colour = "blue", 
                args = list(mean.logGDP, sd.logGDP), size = 1) +
  xlim(c(0, 60000)) +
  theme_bw()

set.seed(111)

runs <- 100000 # number of random draws
siteA <- rbeta(runs, 20, 100) # random draws from siteA dist
siteB <- rbeta(runs, 40, 115) # random draws from siteB dist
pval.sim <- sum(siteA > siteB)/runs # Identify the p-value of siteA > siteB

ggplot(data.frame(diff = siteA/siteB), aes(diff, ..density..)) +
  geom_histogram(stat="bin", binwidth=0.1, fill = "lightblue", colour = "black") +
  theme_bw()



 

mutate(someting = (close - lag(close, 30))/close)


