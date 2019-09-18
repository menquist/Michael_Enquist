library(kernlab) 
library(quantmod)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

options("getSymbols.warning4.0" = FALSE)

from <- "2000-01-01"
# to <- "2011-12-31"

StockData <- getSymbols("XOM", src = "yahoo", from = from,  auto.assign = getOption('getSymbols.auto.assign', FALSE))


Close.zoo <- StockData[,6]
Dates <- time(Close.zoo)
Years <- substr(Dates, 1, 4)
Close <- data.frame(close = as.numeric(Close.zoo))

countYears <- NULL
for(i in 1:length(unique(Years))) {
	count <- 1:table(Years)[[i]]
	countYears[length(countYears)+1:length(count)] <- count
}

Data_byYear <- data.frame(year = as.factor(Years), count = countYears, Close)

castData <- dcast(Data_byYear, count ~ year) %>%
		na.omit() %>%
		select(-count) %>%
		melt(value.name = "close", variable.name = "year")

countYears <- NULL
for(i in 1:length(unique(Years))) {
	count <- 1:table(castData$year)[[i]]
	countYears[length(countYears)+1:length(count)] <- count
}

theData <- data.frame(castData, day = countYears)

Close.std <- theData %>% 
		mutate(close = (close - mean(close))/sd(close)) %>%
		group_by(day, year)

norm.year <- NULL
for(i in 1:length(unique(Years))) {
	norm.year[i] <- filter(Close.std, year == unique(Years)[i] & day == 1)$close	
}

for(j in 1:length(unique(Years))) {
	Close.std$close[which(Close.std$year == as.factor(unique(Years))[j])] <- Close.std$close[which(Close.std$year == as.factor(unique(Years))[j])] - norm.year[j]
}

		
ggplot(Close.std, aes(x = day, y = close)) + geom_line(aes(colour = year))


#train <- select(Close.std, year, day, close) %>%
#		filter(!(year == 2011 & day >= 62))
train <- select(Close.std, year, day, close)
test <- data.frame(year = rep(train$year[length(train$year)], 100),
		 day = (train$day[length(train$day)]+1):(train$day[length(train$day)]+100))

#test <- filter(Close.std, year == 2011 & day >= 62) %>%
#		select(year, day, close)

x.train <- cbind(as.integer(train$year), as.numeric(train$day))
y.train <- as.numeric(train$close)

x.test <- cbind(as.integer(test$year), as.numeric(test$day))

mod <- gausspr(x = x.train, y = y.train)
pred <- predict(mod, newdata = x.test, type = "response") 

Predicted <- data.frame(year = test$year, day = test$day, close = pred)

plot_Dat <- train %>%
		filter(year == 2015) %>%
		ggplot(aes(x = day, y = close)) + geom_line() +
		geom_line(data = Predicted, colour = "blue")
		#geom_line(data = test, colour = "red")
print(plot_Dat)

