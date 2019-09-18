# load library

library(dplyr)
library(rCharts)
library(RColorBrewer)
library(ggplot2)
library(devtools)
# wrangle that data with dplyr and group
# settings will extend to the plot!
gdp.delta.new <- gapminder %>%
  select(year, continent, country, gdpPercap) %>%
  mutate(change = 100*((gdpPercap - lag(gdpPercap)))/gdpPercap) %>%
  filter(year > 1952) %>%
  group_by(continent, year) %>%
  summarise(mean.growth = mean(change))

# construct the plot frame
plot.frame <- ggplot(gdp.delta.new, aes(x = year, y = mean.growth))

# add a point layer
plot2 <- plot.frame + geom_line(aes(colour = continent))

# add axis labels and a title
plot3 <- plot2 + 
  ggtitle("Mean growth of GDP per Capita by Continent \n ~ 1992 - 2007 ~") +
  xlab("Year") + ylab("Life Expectancy")

# print the plot
print(plot3)



library(rCharts)
library(RColorBrewer) # for pretty colours
library(gapminder)

# wrangle with your typical dplyr
dat <- gapminder %>%
  group_by(continent, year) %>%
  filter(continent != "Oceania") %>% # why do I remove Oceania?
  summarise(mean.lifeExp = mean(lifeExp)) %>% 
  select(continent, mean.lifeExp, year)

# create the plot
c1 <- nPlot(mean.lifeExp ~ year, 
            group = "continent", 
            data = dat, 
            type = "stackedAreaChart")

# add the so called "aesthetics" or properties
c1$chart(color = brewer.pal(6, "Set2")) # colour from RColorBrewer library
c1$yAxis(tickFormat= "#!d3.format(',.1f')!#")
c1$yAxis(axisLabel = "Life Expectancy", width = 62)
c1$xAxis(axisLabel = "Year")
c1$chart(tooltipContent = "#! function(key, x, y){
         return '<h3>' + key + '</h3>' + 
         '<p>' + y + ' years ' + x + '</p>'
         } !#")

