index <- read.csv("index_dat.csv.txt")[,-1]
library(plry)
library(dplry)


head(index)




summary(index)
pairs(index)
cor(index)

regress <- lm(HK ~ JP + SP + FT, data = index)
summary(regress)
plot(regress)

head(regress)
a <- regress$res
hist(a)
plot(a)
HK <- index %>% 
  mutate(long = ifelse(HK2 < 0.005,0,1)) %>%
  mutate(short = ifelse(HK2 > 0.005,0,1)) 
  
head(HK)
length(HK$long) # 2473
mat1 <- sum(HK$long) # 848
mat2 = length(HK$long)-mat1
  
logicR <- glm(long ~ JAP + S_P + FTSE, data= HK, family = "binomial")
logicR
summary(logicR)
plot(logicR)
str(logicR)
predict(logicR)

pred <- predict(logicR)
pred2 <- ifelse(pred < 0.005, 0,1)

length(pred2) #2473
mat3 <- sum(pred2) # 280 ones
mat4 <- length(pred2)-sum(pred2)
mat1
mat2
mat3
mat4
a1 <- mat1/length(HK$long)