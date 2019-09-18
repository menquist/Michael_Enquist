## read in data
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv") ## view the first few rows of the data
head(mydata)
## summary statistics for variables (3M, Quantiles)
summary(mydata)
## find standard deviation of variables
sapply(mydata, sd)
## declare rank as a categorical variable
mydata$rank <- factor(mydata$rank) ## build logistic regression model
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
## produce output for logistic regression model
summary(mylogit)
head(mylogit)

## hold GRE and GPA at their means
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4))) ## view new data frame
newdata1
summary(newdata1)
## introduce new variable rankP: probability of acceptance into grad school
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
#view probability table
newdata1
summary(newdata1)
