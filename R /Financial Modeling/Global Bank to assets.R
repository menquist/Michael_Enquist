library(WDI) # World Bank API
library(MASS) # package for fitdistr

# retrieve Bank capital to assets ratio from World Bank api
capital_to_assets <- WDI(country="all", indicator = "FB.BNK.CAPA.ZS") %>%
  setNames(c("iso2c", "country", "ratio", "year")) %>% 
  na.omit()

# fit the distribution based on maximum likelihood estimate
parameters <- fitdistr(capital_to_assets$ratio, "log-normal")
mean.log <- parameters$estimate[[1]]
sd.log <- parameters$estimate[[2]]

# histogram for the bank capital to assets ratio and overlayed
# fitted density (red) and theoretical lognormal density (blue)
ggplot(capital_to_assets, aes(ratio)) +
  geom_histogram(stat="bin", binwidth=1, fill = "lightblue", colour="black", aes(y=..density..)) +
  ggtitle("Global Bank Capital to Assets Ratio \n fitted dist (red) & theoretic dist (blue)") +
  geom_density(colour="red", size = 1) + 
  stat_function(fun = dlnorm, colour = "blue", 
                args = list(mean.log, sd.log), size = 1) +
  theme_bw()

# qqplot - lognormal capital to asset ratio data vs theoretical lognormal
qqplot(qlnorm(ppoints(capital_to_assets$ratio), mean.log, sd.log), 
       capital_to_assets$ratio, col = "blue", ylab="Lognormal Data", 
       xlab="Theoretical Distrubution")

# add the qqline fit to the qqplot
qqline(capital_to_assets$ratio, 
       distribution = function(p) qlnorm(p, mean.log, sd.log), 
       lwd = 2)

