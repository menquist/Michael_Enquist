library(quantmod)
library(plyr)
library(dplyr)

# Gather data
from.dat <- as.Date("01/02/88", format="%m/%d/%y")
to.dat <- as.Date("09/30/98", format="%m/%d/%y")

HK2 <- getSymbols("^HSI", from = from.dat, to = to.dat, auto.assign = FALSE)[,6]
JAP <- getSymbols("^N225", from = from.dat, to = to.dat, auto.assign = FALSE)[,6]
S_P <- getSymbols("^GSPC", from = from.dat, to = to.dat, auto.assign = FALSE)[,6]
FTSE <- getSymbols("^FTSE", from = from.dat, to = to.dat, auto.assign = FALSE)[,6]

#Checking the structure and head for each object

str(HK2)
head(HK2)
str(JAP)
head(JAP)
str(S_P)
head(S_P)
str(FTSE)
head(FTSE)

#Changing the data frame from .xts to numberic as a new object.

HK2_new <- data.frame(date = as.factor(time(HK2)), HK2 = as.numeric(HK2))
JAP_new <- data.frame(date = as.factor(time(JAP)), JAP = as.numeric(JAP))
S_P_new <- data.frame(date = as.factor(time(S_P)), S_P = as.numeric(S_P))
FTSE_new <- data.frame(date = as.factor(time(FTSE)), FTSE = as.numeric(FTSE))

#Checking the structure and head for each new object
str(HK2_new)
head(HK2_new)
str(JAP_new)
head(JAP_new)
str(S_P_new)
head(S_P_new)
str(FTSE_new)
head(FTSE_new)


#inner join 

x1 <- inner_join(HK2_new, JAP_new, by = "date")
x2 <- inner_join(x1,S_P_new, by = "date")
data <- inner_join(x2, FTSE_new, by = "date") %>%
  mutate(HK2 = (HK2 - lag(HK2))/HK2, 
         JAP = (JAP - lag(JAP))/JAP,
         S_P = (S_P - lag(S_P))/S_P,
         FTSE = (FTSE - lag(FTSE))/FTSE) %>%
  filter(date != "1988-01-04") %>%
  select(-date)

data$HK2 <- as.numeric(Next(data$HK2, k = 1))
names(data)[names(data)=="Next"] <- "HK2"

data$JAP <- as.numeric(Next(data$JAP, k = 1))
names(data)[names(data)=="Next"] <- "JAP"
data <- data[-dim(data)[1],]

data <- data[-dim(data)[1],][-dim(data)[1],]

summary(data)


# Multiple Regression
mod <- lm(HK2 ~ ., data)
summary(mod)
cor(data)

# Logistic Indicator
data_logit <- data %>%
  mutate(long = ifelse(HK2 < 0.005, 0, 1)) %>%
  mutate(short = ifelse(HK2 > -0.005, 0, 1))

# Predict long
logit_long <- glm(long ~ JAP + S_P + FTSE, data = data_logit, family = "binomial")

pred_long <- predict(logit_long, type = "response")
pred_out_long <- ifelse(pred_long > 0.5, 1, 0)

class_error_long <- length(which(data_logit$long != pred_out_long))/length(pred_out_long)
print(class_error_long)
class_correct_long <- length(which(data_logit$long == pred_out_long))/length(pred_out_long)
print(class_correct_long)

# confusion matrix
table(data.frame(predicted = pred_long > 0.5, actual = data_logit$long > 0.5))



# Predict short
logit_down_short <- glm(as.factor(short) ~ JAP + S_P + FTSE, data = data_logit, family = "binomial")

pred_short <- predict(logit_down_short, type = "response")
pred_out_short <- ifelse(pred_short < -0.5, 1, 0)

class_error_short <- length(which(data_logit$short != pred_out_short))/length(pred_out_short)
print(class_error_short)
class_correct_short <- length(which(data_logit$short == pred_out_short))/length(pred_out_short)
print(class_correct_short)

# confusion matrix
table(data.frame(predicted = pred_short > 0.5, actual = data_logit$short > 0.5))
