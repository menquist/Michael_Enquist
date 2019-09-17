library(Quandl)
Quandl.api_key("c58SRnkLsYYrxJs6d74e")

### Cost to register property (% of property value) - Hong Kong SAR, China
Percent.of.property.value.HK <- Quandl("OPEC/ORB", start_date="1900-01-01", end_date="2017-03-31", collapse="daily")
ggplot(Percent.of.property.value.HK, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(Percent.of.property.value.HK)[2] <- c("Oil.Price") 
str(Percent.of.property.value.HK)
BKRHUGHES/RIGS_BY_PROV_TOTAL_CANADA_LAND

ggplot(Percent.of.property.value.HK, aes(y = Oil.Price, x = Date)) + geom_line(size = 1, colour = "blue") +scale_shape_manual(values=c(date), guide = guide_legend(nrow=1)) +
   labs(x="Year",y="Oil Price In USD") + theme_bw(base_size = 18, base_family = "serif") + 
  ggtitle("The Price of Oil for the Past 15 Years ") + theme(plot.title = element_text(hjust = 0.5))

### Canada rig count
Canada.Rig.Count <- Quandl("BKRHUGHES/RIGS_BY_PROV_TOTAL_CANADA_LAND", start_date="2000-01-01", end_date="2017-03-31", collapse="daily")
ggplot(Canada.Rig.Count, aes(y = Value, x = Date)) + geom_line(size = 1, colour = "blue") + theme_bw()
colnames(Canada.Rig.Count)[2] <- c("Rig.Count") 



SP <-  getSymbols(Symbols = "RRX.TO", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
RRX.TO <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.HKSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

#SP <-data.frame(date=as.character(index(SP)),coredata(SP))
#SP.adj <- SP %>% 
#  mutate(HSKE = 100*((SP[2] - lag(SP[2])))/SP[2])
#SP.adj <- na.omit(SP.adj[-2])

SP <-  getSymbols(Symbols = "ZEO.TO", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
ZEO.TO <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.HKSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)


SP <-  getSymbols(Symbols = "ZJO.TO", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
ZJO.TO <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.HKSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

SP <-  getSymbols(Symbols = "^GSPTSE", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
TSX <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.HKSE = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

SP <-  getSymbols(Symbols = "^GSPC", src = "yahoo",from=year, auto.assign = getOption('getSymbols.auto.assign',FALSE))[,6]
SP.1 <- data.frame(date = as.factor(time(SP)), price = as.numeric(SP))  %>%
  mutate(rate.SP = (price - lag(price))/lag(price)) %>%
  filter(date != year) %>%
  select(-price)

colnames(SP.1) <- c("Date","S&P")
colnames(RRX.TO) <- c("Date","RRX.TO")
colnames(ZEO.TO)<- c("Date","ZEO.TO")
colnames(ZJO.TO) <- c("Date","ZJO.TO")
colnames(TSX) <- c("Date","TSX")



str(Percent.of.property.value.HK)
L <- list(SP.1,RRX.TO,ZEO.TO,ZJO.TO,TSX)
str(L)
Raging.River <- Reduce(inner_join,L)
cor(Raging.River[2:6])



data_oil <- data_oil %>% 
  mutate(Date = as.Date.character(Date))

codes <- c("OPEC/ORB", "BKRHUGHES/RIGS_BY_PROV_TOTAL_CANADA_LAND")
x1 <- Quandl(codes,
             start_date="2000-01-01",collapse="daily")
str(x1)
head(x1)
data.oil<-data.frame(Date = index((x1)),coredata(x1))
x1$Date <- as.POSIXct(paste(x1$Date, as.character(x1$Date)))
str(data.oil)

Raging.River$Date <- as.POSIXct(paste(Raging.River$Date, as.character(Raging.River$Date)))
str(Raging.River)

L <- list(Oil.metrics,Raging.River)
Oil.metrics <- Reduce(inner_join,L)
str(L)
write.csv(Percent.of.property.value.HK,"Oil_Price.2017.01.01.csv")


river <- read.csv("Raging.River.csv", header = T)
str(river)
oil <- read.csv("Oil.metrics.csv", header = T)
oil.price <- read.csv("Oil_Price.2017.01.01.csv", header = T)
str(oil.price)
str(oil)
collect <- oil.price  %>%
  mutate(rate.Oil.Price = (Oil.Price - lag(Oil.Price))/lag(Oil.Price)) %>% 
  select(-Oil.Price)



combind <- inner_join(river,collect, by="Date")
str(combind)

collect <- na.omit(combind)

str(collect)
cor(collect[2:7])
pairs(collect[2:7])
Raging.River <- collect %>% 
  select(-Date,-S.P)
str(Raging.River)
mod <- lm(RRX.TO~., data = Raging.River)
summary(mod)
plot(mod)

### Testing serial correlation
res = mod$res 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



library(ResourceSelection)
hoslem.test(as.numeric(Raging.River$RRX.TO), fitted(mod))

data_logit <- Raging.River %>%
  mutate(short = ifelse(RRX.TO > 0, 0, 1)) %>%
  mutate(long = ifelse(RRX.TO < 0, 0, 1)) %>% 
  select(-RRX.TO)

str(data_logit)
# Predict long
testlogit <- data_logit[,1:4]#
longlegit <- data_logit[,6]
logitmerge <- cbind(testlogit,longlegit)

logit_long <- glm(longlegit ~ ., data = logitmerge, family = "binomial")

summary(logit_long)
plot(logit_long)
str(logit_long)


library(glmnet)

model<-glmnet(as.matrix(testlogit), as.integer(longlegit), family="binomial")
coef<-predict(model, type="coefficients")
lasso <- cv.glmnet(as.matrix(testlogit),as.integer(longlegit), family="binomial") 
lasso_coef<-predict(lasso, type="coefficients")
plot(model, label = TRUE)
plot(lasso)
plot(coef)
plot(lasso_coef)
summary(lasso_coef)
str(logitmerge)
library(ResourceSelection)
hoslem.test(as.numeric(logitmerge$longlegit), fitted(mod))

#------------------ run LOOCV -----------------------------
# initialise prediction vector
isCorrect <- NULL

dat.bc <- logitmerge

# perform cv
for(i in 1:nrow(dat.bc))
{
  x.train <- dat.bc[-i,-1] # remove ID
  x.test <- dat.bc[i,-1] # remove ID
  
  mod <- glm(longlegit ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$longlegit
  isCorrect[i] <- ifelse(y.pred == y.true, 1, 0)
}

# misclassification error
sum(isCorrect)/length(isCorrect)

#------------------ run K-fold -----------------------------
error <- NULL
nRuns <- 10000
kSize <- 600

dat.bc <- data_logit

# perform k-fold
for(i in 1:nRuns)
{
  randomSample <- sample(1:nrow(dat.bc), kSize, replace = FALSE)
  x.train <- dat.bc[randomSample, -1] # remove ID
  x.test <- dat.bc[-randomSample, -1] # remove ID
  
  mod <- glm(longlegit ~ ., x.train, family = "binomial")
  pred <- predict(mod, newdata = x.test, type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- x.test$longlegit
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)  
}

# plot error over iterations
plot(cumsum(error)/seq_along(error), type = "l")

# fit model
mod1 <- glm(longlegit ~ ., dat.bc[,-1], family = "binomial") # remove ID

#---------------- Bootstrapping ----------------------------
# quantify variance in your model
# set params
error <- NULL
nRuns <- 50
sampleSize <- 50

for(i in 1:nRuns)
{
  subsetDat <- dplyr::sample_n(dat.bc, sampleSize)
  pred <- predict(mod1, newdata = subsetDat[,-1], type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- subsetDat$longlegit
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)
}

mse <- mean(error^2)
mse

df <- dat.bc
head(df)
str(df)
#Look up ?factor

df$longlegit <- factor(df$longlegit, levels=c(0,1), labels = c("Down","Up"))




fit.logit <- glm(longlegit~., data=df, family = binomial())
summary(fit.logit)

prob <- predict(fit.logit, df, type="response")
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE), labels = c("Down","Up"))
#By default predict() function predicts the log odds of having a malignant outcome

logit.perf <- table(df$longlegit, logit.pred, dnn = c("Actual", "Predicted") )
logit.perf
