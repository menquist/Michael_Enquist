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

gaminder
gapminder
head(ga)

head(gapminder)
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

select(gapminder,1,3,5) %>%
filter(gapminder, gdpPercap > 1200, year == 2007,) 

dat <- gapminder %>%
  group_by(year) %>%
  summarize(mean(pop), sum(pop))
dat

  select(country,gdpPercap,year) %>%
  filter(gdpPercap > 1200) %>%
  mutate(gdpPercap.diff=diff(gdpPercap)) %>%
  group-by(year)

dat <- gapminder %>%
  group_by(continent) %>%
  summarize(mean(pop), sum(pop))

dat <- gapminder %>%
  group_by(continent, year, gdpPercap) %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952) %>%
  summarize(mean(pop), sum(pop))
dat

dat.cont <- gapminder %>%
  select( year, gdpPercap, continent) %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952) %>%
  group_by(year) %>%
  summarize(change_gdpPercap = mean(change))
dat.cont  


library(ggplot2)

plot <- ggplot(dat.cont , aes(year, change_gdpPercap) + geom_line(colour= "blue") + geom_point(colour= "red"))
plot 

plot1 <- ggplot(dat.cont , aes(x = year, y = change_gdpPercap, colour = continent) + geom_line() + geom_point())
plot1

head(gapminder)

look <- gapminder %>%
  filter(country %in% "United States", "Cambodia", "Cuba", "Thailand", "Japon", "Vietnam", "China" %%) 
look

plot2 <- ggplot(look , aes(x = year, y = gdpPercap , colour = country) + geom_line())
plots


