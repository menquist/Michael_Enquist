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


######### experimentation
XYZ <- c(l[1]*(1+y[7]),l[2]*(1+(y[2]/100)),l[3]*(1+(y[3]/100)),l[4]*(1+(y[4]/100)),l[5]*(1+(y[5]/100)),l[6]*(1+(y[6]/100)))
XYZ           


y
XYZZ

XYZZ <- c(l[1]+(1+y[7]),l[2]+(1+(y[2])),l[3]+(1+(y[3])),l[4]+(1+(y[4])),
         l[5]+(1+(y[5])),l[6]+(1+(y[6])))
XYZZ
l


XYZZZ <- c((l[1,1]+exp(x[1,1])),(l[1,2]+exp(x[1,2])-x[1,1]),l[3],l[4],l[5],(l[1,6]+exp(x[1,6])-x[1,1]))
XYZZZZ <- c((l[1,1]+exp(x[1,1])),(l[1,2]+exp(x[1,2])),l[3],l[4],l[5],(l[1,6]+exp(x[1,6])))
XYZZZZZ <- c((l[1,1]*exp(x[1,1])),(l[1,2]*exp(x[1,2])),l[3],l[4],l[5],(l[1,6]*exp(x[1,6])))

XYZ
XYZZ
XYZZZ
XYZZZZ
XYZZZZZ


######################## Lambda exp when number 1 increases

n <- c((l[1,1]*exp(x[1,1])),(l[1,2]*exp(x[1,2])),l[3],l[4],l[5],(l[1,6]*exp(x[1,6])))
n1 <- c((n[1]*exp(x[2,7])),n[2]*exp(x[2,1]),n[3]*exp(x[2,3]),n[4],n[5],n[6])
n2 <- c((n1[1]*exp(x[3,7])),n1[2]*exp(x[3,2]),n1[3]*exp(x[3,1]),n1[4]*exp(x[3,4]),n1[5],n1[6])
n3 <- c((n2[1]*exp(x[4,7])),n2[2],n2[3]*exp(x[4,3]),n2[4]*exp(x[4,1]),n2[5]*exp(x[4,5]),n2[6])
n4 <- c(n3[1],n3[2],n3[3],n3[4]*exp(x[5,4]),n3[5]*exp(x[5,1]),n3[6]*exp(x[5,6]))
n5 <- c(n4[1],n4[2],n4[3],n4[4],n4[5]*exp(x[6,5]),n4[6]*exp(x[6,1]))

######################## Lambda log whez zumber 1 izcreases

z <- c((l[1,1]+exp(x[1,1])),(l[1,2]+exp(x[1,2])),l[3],l[4],l[5],(l[1,6]+exp(x[1,6])))
z1 <- c((z[1]+exp(x[2,7])),z[2]+exp(x[2,1]),z[3]+exp(x[2,3]),z[4],z[5],z[6])
z2 <- c((z1[1]+exp(x[3,7])),z1[2]+exp(x[3,2]),z1[3]+exp(x[3,1]),z1[4]+exp(x[3,4]),z1[5],z1[6])
z3 <- c((z2[1]+exp(x[4,7])),z2[2],z2[3]+exp(x[4,3]),z2[4]+exp(x[4,1]),z2[5]+exp(x[4,5]),z2[6])
z4 <- c(z3[1],z3[2],z3[3],z3[4]+exp(x[5,4]),z3[5]+exp(x[5,1]),z3[6]+exp(x[5,6]))
z5 <- c(z4[1],z4[2],z4[3],z4[4],z4[5]+exp(x[6,5]),z4[6]+exp(x[6,1]))



######################################################################################################### Multiresponse Gaussian Family




g <- c(l[1,1]+log(exp(x[1,1]*(1+v[1,1]))),l[1,2]+log(exp(x[1,2]*(1+v[1,2]))),l[3],l[4],l[5],
       l[1,6]+log(exp(x[1,6]*(1+v[1,6]))))
g1 <- c(g[1]+log(exp((x[2,7]*(1+v[2,7])))),g[2]+log(exp((x[2,1]*(1+v[2,1])))),g[3]+log(exp((x[2,3]*(1+v[2,3])))),g[4],g[5],g[6])
g2 <- c(g1[1]+log(exp((x[3,7]*(1+v[3,7])))),g1[2]+log(exp((x[3,2]*(1+v[3,2])))),g1[3]+log(exp((x[3,1]*(1+v[3,1])))),g1[4]+log(exp((x[3,4]*(1+v[3,4])))),g1[5],g1[6])
g3 <- c(g2[1]+log(exp((x[4,7]*(1+v[4,7])))),g2[2],g2[3]+log(exp((x[4,3]*(1+v[4,3])))),
        g2[4]+log(exp((x[4,1]*(1+v[4,1])))),
        g2[5]+log(exp((x[4,5]*(1+v[4,5])))),g2[6])
g4 <- c(g3[1],g3[2],g3[3],g3[4]+log(exp((x[5,4]*(1+v[5,4])))),g3[5]+log(exp((x[5,1]*(1+v[5,1])))),
        g3[6]+log(exp((x[5,6]*(1+v[5,6])))))
g5 <- c(g4[1],g4[2],g4[3],g4[4],g4[5]+log(exp((x[6,5]*(1+v[6,5])))),g4[6]+log(exp((x[6,1]*(1+v[6,1])))))

l                                          
ab