library(devtools)
devtools::install_github("jennybc/gapminder")

fac <- as.factor(rep(c("male", "female"), 5))
num <- seq(1,10,1)
str <- "hello"

#Run a matrix
mat <- matrix(cbind(fac,num,str), ncol =3)

#run a dataframe
df <- data.frame(cbind(fac,num,str))

#Do you see the difference?

cbind(fac,num,str)
rbind(fac,num,str)

library(plyr)

library(dplyr)
library(ggplot2)

library(gapminder)
head(gapminder)
str(gapminder)
list(gapminder)
a <- gapminder$year

b <- gapminder[,5]
c <- gapminder[,1]
max(a)
max(b)
max(c)

#Looking at year and population
dat <- gapminder %>%
  group_by(year) %>%
  summarize(mean(pop), sum(pop))
dat


# Looking into country, gdp, year greater than 1200 gdpercap
dat.1 <- gapminder %>%
  select(country,gdpPercap,year) %>%
  filter(gdpPercap > 1200) %>%
  mutate(gdpPercap.diff=diff(gdpPercap)) %>%
  group-by(year)
dat.1

library(reshape2)
#Looking into each continent and population
dat.2 <- gapminder %>%
  select(year, gdpPercap, country)
dat.2 <- melt(dat.2, id=c("year","gdpPercap", "country"))  



#Lookign into (continent, year, gdpPercap) over the years
dat.3 <- gapminder %>%
  group_by(continent, year, gdpPercap) %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952) %>%
  summarize(mean(pop), sum(pop))
dat.3


# looking at the averages from 1952 respect to gdppercap
dat.cont <- gapminder %>%
  select( year, gdpPercap, continent) %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952) %>%
  group_by(year) %>%
  summarize(change_gdpPercap = mean(change))
dat.cont  


# trying to elimate Oceania from the dataset
gDat <- gapminder %>%
  select(continent, year, pop) %>%
  filter(year == 2007) %>%
  filter(continent != "Oceania") %>% 
  group_by(pop) 
gDat
#Ploting each Continent, sooo pretty!!!!
p1 <- ggplot(gDat, aes(log(pop), fill = continent)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~continent) + 
  ggtitle("Distribution of Population by Continent")
print(p1)


#selecting year 1992 and 2007, and eliminating out Oceania
hDat <- gapminder %>%
  select(continent, year, lifeExp) %>%
  filter(year %in% seq(1992, 2007, 5)) %>%
  filter(continent != "Oceania") %>% 
  group_by(lifeExp)

hDat


# Ploting life expectancy over the years
p2 <- ggplot(hDat, aes(year, lifeExp, colour = continent)) + 
  geom_jitter() + 
  ggtitle("Life expectancy of each Continent from 1990 to 2005")
print(p2)

p2 + stat_smooth(method = "lm", se = FALSE)


# Looking into Africa
iDat <- gapminder %>%
  select(continent, lifeExp, year,gdpPercap,country) %>%
  filter(continent == "Africa") %>%
  group_by(continent)
iDat

pi <- ggplot(iDat, aes(x = year, y = lifeExp, colour = country)) + 
  geom_jitter(aes(size = gdpPercap)) +
  theme(legend.position="none") +
  ggtitle("Life expectancy by year for African Countries Respect to GDP per Capita")
print(pi)


w# Mutating variables and looking into Rwanda
jDat <- gapminder %>%
  filter(country == "Rwanda") %>%
  mutate(change.pop = 100*((pop - lag(pop)))/pop) %>%
  mutate(change.lifeExp = 100*((lifeExp - lag(lifeExp)))/lifeExp) %>%
  mutate(change.gdpPercap = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  select(country, year, change.pop, change.lifeExp, change.gdpPercap) %>%
  filter(year > 1952) %>%
  group_by(year)


p3 <- ggplot(jDat, aes(y = change.pop, x = year)) + 
  geom_line(colour = "magenta") +
  ggtitle("Population Growth of Rwanda \n ~ 1957 - 2007 ~") +
  ylab("Population Differential")
p3

p4 <- ggplot(jDat, aes(y = change.lifeExp, x = year)) +
  geom_line(colour = "blue") +
  ggtitle("Life Expectancy Differential of Rwanda \n ~ 1957 - 2007 ~") +
  ylab("Life Expectancy Differential")
p4

p5 <- ggplot(jDat, aes(y = change.gdpPercap, x = year)) +
  geom_line(colour = "orange") +
  ggtitle("GDP per Capita Growth Rate of Rwanda \n ~ 1957 - 2007 ~") +
  ylab("GDP Differential")
p5

#Lastly looking at each country with respect to life expectancy 
kDat <- gapminder %>%
  filter(year %in% c(1952, 1982, 2007)) %>%
  group_by(year)
kDat

#Plot this out and see any differences between life expectancy over the years.
#Also using a Lognormal the dataset. stat_smooth(method = "lm", size = 1, colour = "black", se = F)
ggplot(kDat, aes(y = lifeExp, x = log(gdpPercap), 
                 colour = as.factor(year))) + 
  geom_point(alpha = 0.6, size = 4) +
  stat_smooth(method = "lm", size = 1, colour = "black", se = F) +
  ggtitle("Life expectancy years 1952, 1982, and 2007")


gdp.delta <- gapminder %>%
  select(year, country, gdpPercap, lifeExp) %>%
  filter(year > 1952) %>%
  group_by(country, year, lifeExp) %>%
  summarise(gdp_median = median(gdpPercap))

gdp.median <- dcast(gdp.delta, country ~ year, value.var="gdp_median")
#gdp.delta <- data.frame(t(gdp.delta))
str(gdp.median)

Rwanda.1997 <- gdp.median$`1997`
str(Rwanda.1997)
#library(tidyr)

#country <-  gapminder %>% 
  #select(country, year, gdpPercap,lifeExp) %>% 
  #spread(year, gdpPercap)
#country
#str(country)
#pairs(country)
#doing a cross validation on africa (idat)

# Looking into lifeExp and gdpPercap
#Life.GDP <- gapminder %>%
#  select( lifeExp, gdpPercap) 


# Now using a cross validation between lifeExp and gdpPercap

# Looking into lifeExp and gdpPercap
Life.GDP <- gapminder %>%
  select( lifeExp, gdpPercap) 


# Now using a cross validation between lifeExp and gdpPercap

train <- Life.GDP[1:565,]
test <- Life.GDP[566:1704,]

Trainx <- train[sample.int(dim(train)[1], size=565), ]
Testx <- test[sample.int(dim(test)[1], size=1139), ]
dim(Trainx)      
dim(Testx)

x.train <- sample_n(train,565)
x.test <- sample_n(test, 1139)
dim(x.train)
dim(x.test)

#Running a regression for train
A <- lm(lifeExp ~ gdpPercap, data=Trainx)
summary(A)
anova(A)
#Running the regression for test
B <- lm(lifeExp ~ gdpPercap, data=Testx)
summary(B)
anova(B)
library(car)

plot(residuals(A))

crPlots(A)
plot(fitted(A), residuals(A), xlab="Fitted", ylab= "residuals")
abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
plot(fitted(A), abs(residuals(A), xlab= "Fitted", main="|residuals|"))
     
     
     summary(lm((residuals(A) ~ fitted(A))))
             
             qqnorm(residuals(A), ylab="residuals")
             qqline(residuals(A), col= "red", lwd = 2)  
             hist(residuals(A))
             
             Z <- lm(lifeExp~gdpPercap, data=Trainx)
             summary(Z)
             anova(Z)
             plot(residuals(Z))
             
             crPlots(Z)
             
             plot(fitted(Z), residuals(Z), xlab="Fitted", ylab= "residuals")
             abline(h= c(-2,0,2), lwd = c(2,2,2), col=c("red","red", "red"), lty= c(2,1,2) )
             plot(fitted(Z), abs(residuals(Z), xlab= "Fitted", main="|residuals|")
                  summary(lm((residuals(Z) ~ fitted(Z))))
                          qqnorm(residuals(Z), ylab="residuals")
                          qqline(residuals(Z), col= "red", lwd = 2)  
                          hist(residuals(Z))
                          
                          predict(Z) 
                          pred <- predict(Z, newdata = x.test)
                          obs <- x.test$gdpPercap
                          plot(obs ~ pred)
                          abline(c(0,1), col="red", lwd = 2)
                          RVSE <- sqrt(mean(pred - obs)^2)
                          AE <- sum(abs(pred - obs))
                          acf(pred)
                          pacf(pred)
                          summary(pred)
                          #prediction
                          #1 hold out set
                          #2 k-fold CV
                          #3 LOOCV
                          #4 bootstrape
                          
                          #sqr = seq(1,100)
                          #sqr.squared = NULL
                          #for(n in 1:75)
                          #{sqr.squared[n] = sqr[n]^2}
                          
                          #summary(sqr.squared)
                          #plot(sqr.squared)
                          

                        
                          
                          
