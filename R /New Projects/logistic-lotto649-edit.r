setwd("~/Downloads")
lotto <- read.csv("649.csv")
str(lotto)
#num.1 <- as.numeric(lotto$NUMBER.DRAWN.7)

###
lottomax <- lotto[!duplicated(lotto$DRAW.DATE),]

lotto <- lottomax %>% 
  select(NUMBER.DRAWN.1,NUMBER.DRAWN.2,NUMBER.DRAWN.3,NUMBER.DRAWN.4,NUMBER.DRAWN.5,NUMBER.DRAWN.6)

dat1 <- as.data.frame(lotto)
nr <- nrow(dat1)
dat1 <- (dat1[-1,] - dat1[-nr,]) / dat1[-nr,]

test <- sapply(dat1, function(col) ifelse(col>0,1,ifelse(col<=0,0,col)))
test <- data.frame(test)

data <- data.frame(dat1)


##############################################################################################################

#'''test is the binomial and data is ROC of the lotto numbers'''
data_logit1 <- data[,-c(1)]
logit_long1 <- glm(test[,1] ~ .,na.action(na.omit), data = data_logit1, family = "binomial")
c <- c(0,logit_long1$coefficients)
summary(logit_long1)   

data_logit2 <- data[,-c(2)]
logit_long2 <- glm(test[,2] ~ .,na.action(na.omit), data = data_logit2, family = "binomial")
c1 <- c(logit_long2$coefficients)
summary(logit_long2) 

#cx <- cbind(c,c1,c2)

data_logit3 <- data[,-c(3)]
logit_long3 <- glm(test[,3] ~ .,na.action(na.omit), data = data_logit3, family = "binomial")
c2 <- c(logit_long3$coefficients)
summary(logit_long3) 

data_logit4 <- data[,-c(4)]
logit_long4 <- glm(test[,4] ~ .,na.action(na.omit), data = data_logit4, family = "binomial")
c3 <- c(logit_long4$coefficients)
summary(logit_long4) 


data_logit5 <- data[,-c(5)]
logit_long5 <- glm(test[,5] ~ .,na.action(na.omit), data = data_logit5, family = "binomial")
c4 <- c(logit_long5$coefficients)
summary(logit_long5) 


data_logit6 <- data[,-c(6)]
logit_long6 <- glm(test[,6] ~ .,na.action(na.omit), data = data_logit6, family = "binomial")
c5 <- c(logit_long6$coefficients)
summary(logit_long6) 



c <- (data.frame(logit_long1$coefficients))
#c$ID <- 1:nrow(c)
c <- cbind(ID = rownames(c), c)
rownames(c) <- 1:nrow(c)
c <-dcast(melt(c, id.vars = "ID"), variable ~ ID)


c1 <- data.frame(logit_long2$coefficients)
#c1$ID <- 1:nrow(c1)
c1 <- cbind(ID = rownames(c1), c1)
rownames(c1) <- 1:nrow(c1)
c1 <-dcast(melt(c1, id.vars = "ID"), variable ~ ID)


c2 <- data.frame(logit_long3$coefficients)
#c2$ID <- 1:nrow(c2)
c2 <- cbind(ID = rownames(c2), c2)
rownames(c2) <- 1:nrow(c2)
c2 <-dcast(melt(c2, id.vars = "ID"), variable ~ ID)


c3 <- data.frame(logit_long4$coefficients)
#c3$ID <- 1:nrow(c3)
c3 <- cbind(ID = rownames(c3), c3)
rownames(c3) <- 1:nrow(c3)
c3 <-dcast(melt(c3, id.vars = "ID"), variable ~ ID)

c4 <- data.frame(logit_long5$coefficients)
#c3$ID <- 1:nrow(c3)
c4 <- cbind(ID = rownames(c4), c4)
rownames(c4) <- 1:nrow(c4)
c4 <-dcast(melt(c4, id.vars = "ID"), variable ~ ID)

c5 <- data.frame(logit_long6$coefficients)
#c3$ID <- 1:nrow(c3)
c5 <- cbind(ID = rownames(c5), c5)
rownames(c5) <- 1:nrow(c5)
c5 <-dcast(melt(c5, id.vars = "ID"), variable ~ ID)


doc <- rbindlist(list(c,c1,c2,c3,c4,c5), fill=T)
summary(logit_long1)
summary(logit_long2)
summary(logit_long3)
summary(logit_long4)
summary(logit_long5)
summary(logit_long6)

#####


l <- lotto[nrow(lotto),]

a = c(l[1,1]+doc[1,2], l[1,2]+doc[1,3])
a
#####

str(doc)

x <- doc[,-1]
y <- c(colSums(x, na.rm = T, dims = 1))




########################### Multiresponse Gaussian Family  add other coefficenets



mod <- lm(data[,1] ~., data=data_logit1)
b <- c(mod$coefficients)
b <- (data.frame(mod$coefficients))
b <- cbind(ID = rownames(b), b)
rownames(b) <- 1:nrow(b)
b <-dcast(melt(b, id.vars = "ID"), variable ~ ID)

mod1 <- lm(data[,2] ~., data=data_logit2)
b1 <- data.frame(mod1$coefficients)
b1 <- cbind(ID = rownames(b1), b1)
rownames(b1) <- 1:nrow(b1)
b1 <-dcast(melt(b1, id.vars = "ID"), variable ~ ID)



mod2 <- lm(data[,3] ~., data=data_logit3)
b2 <- data.frame(mod2$coefficients)
b2 <- cbind(ID = rownames(b2), b2)
rownames(b2) <- 1:nrow(b2)
b2 <-dcast(melt(b2, id.vars = "ID"), variable ~ ID)



mod3 <- lm(data[,4] ~., data=data_logit4)
b3 <- data.frame(mod3$coefficients)
b3 <- cbind(ID = rownames(b3), b3)
rownames(b3) <- 1:nrow(b3)
b3 <-dcast(melt(b3, id.vars = "ID"), variable ~ ID)

mod4 <- lm(data[,5] ~., data=data_logit5)
b4 <- data.frame(mod4$coefficients)
b4 <- cbind(ID = rownames(b4), b4)
rownames(b4) <- 1:nrow(b4)
b4 <-dcast(melt(b4, id.vars = "ID"), variable ~ ID)

mod5 <- lm(data[,6] ~., data=data_logit6)
b5 <- data.frame(mod5$coefficients)
b5 <- cbind(ID = rownames(b5), b5)
rownames(b5) <- 1:nrow(b5)
b5 <-dcast(melt(b5, id.vars = "ID"), variable ~ ID)

doc1 <- rbindlist(list(b,b1,b2,b3,b4,b5), fill=T)

v <- doc1[,-1]

############## results with respect to change

g <- c(l[1,1]+((x[1,1]*(1+v[1,1]/100))),l[1,2]+((x[1,2]*(1+v[1,2]/100))),l[3],l[4],l[5],
       l[1,6]+((x[1,6]*(1+v[1,6]/100))))
g1 <- c(g[1]+(((x[2,7]*(1+v[2,7]/100)))),g[2]+(((x[2,1]*(1+v[2,1]/100)))),g[3]+(((x[2,3]*(1+v[2,3]/100)))),g[4],g[5],g[6])
g2 <- c(g1[1]+(((x[3,7]*(1+v[3,7]/100)))),g1[2]+(((x[3,2]*(1+v[3,2]/100)))),g1[3]+(((x[3,1]*(1+v[3,1]/100)))),g1[4]+(((x[3,4]*(1+v[3,4]/100)))),g1[5],g1[6])
g3 <- c(g2[1]+(((x[4,7]*(1+v[4,7]/100)))),g2[2],g2[3]+(((x[4,3]*(1+v[4,3]/100)))),
        g2[4]+(((x[4,1]*(1+v[4,1]/100)))),
        g2[5]+(((x[4,5]*(1+v[4,5]/100)))),g2[6])
g4 <- c(g3[1],g3[2],g3[3],g3[4]+(((x[5,4]*(1+v[5,4]/100)))),g3[5]+(((x[5,1]*(1+v[5,1]/100)))),
        g3[6]+(((x[5,6]*(1+v[5,6]/100)))))
g5 <- c(g4[1],g4[2],g4[3],g4[4],g4[5]+(((x[6,5]*(1+v[6,5]/100)))),g4[6]+(((x[6,1]*(1+v[6,1]/100)))))



g5
