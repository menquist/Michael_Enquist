
setwd("~/Downloads/Translink/")

t=readRDS("stops_2011.Rds") 


#col.names = colnames(read.table(file ="headers_28_subset.txt", header = T))

#names(t)=col.names

head(t)
str(t)
date <- t$OperationDate

library(corrplot)
x_numberic = t[sapply(t,  is.numeric)]

result <- cor(x_numberic)
corrplot(result, method="number", order="AOE", type = "lower")


library(dplyr)

keywords <- t$Pattern[grep("*041", t$Pattern)]
streetname <- my.data2$StopName[grep("*", my.data2$StopName)]
#         BikeLoaded,BikeUnloaded,VehicleDescription,VehicleClass,VehicleAge,VehicleYear,
#VehicleCapacity,VehicleSittingCapacity,VehicleFuelTyle,VehicleLength,ArriveLoad,LeaveLoad, Ons,Offs
filter.integers.numerics <- t %>% 
  select(StopSeqNo,ArriveLoadCompensated,OnsLoadCompensated,OffsLoadCompensated,
         LeaveLoadCompensated,Pattern,OperationDate, 
         WCLiftActivated,BikeLoaded ,BikeUnloaded ,Conditions,
         Wind.Speed,Visibility,Humidity,Temp,DwellTime,DepartureDelay,
         OnAndOffsCompensated,ArriveDelay) %>%  
  filter( Pattern %in% keywords ) 
str(filter.integers.numerics)
summary(filter.integers.numerics)



#load library
library(dummies)

#I used 100000 obs but its best to reduce to 50000 obs 
#train <- filter.integers.numerics[1:100000,]
#summary(train)
#str(train)
#create a dummy data frame. We’ll convert these categorical variables into numeric using encoding.
new_my_data <- dummy.data.frame(filter.integers.numerics, names = c("Pattern", "Line","TimingPoint",
                                                                    "WCLiftActivated","ActualLeaveTime","ScheduledLeaveTime",
                                                                    "ActualArriveTime","ScheduledArriveTime", "VehicleNo","DayType",
                                                                    "BikeLoaded","BikeUnloaded","VehicleDescription","VehicleClass",
                                                                    "VehicleFuelTyle","VehicleLength","VehicleCapacity","VehicleSittingCapacity","Conditions",
                                                                    "StopName","Hour", "ScheduledArriveTime","OperationDate"))
str(new_my_data)

which(apply(new_my_data, 2, var)==0)
is.na(new_my_data) %>% sum

dse <- as.matrix(new_my_data)
m <- as.data.frame(new_my_data)



str(m)

ArriveDelay <- m$ArriveDelay

Data.1 <- m[1:10]
Data.2 <- m[11:20]
Data.3 <- m[21:30]
Data.4 <- m[31:40]
Data.5 <- m[41:50]
Data.6 <- m[51:60]
Data.7 <- m[61:70]
Data.8 <- m[71:77]
Data.9 <- m[81:90]
Data.10 <- m[91:100]
Data.2 <- m[101:20]
Data.3 <- m[21:30]
Data.4 <- m[31:40]
Data.5 <- m[41:50]
Data.6 <- m[51:60]
Data.7 <- m[61:70]
Data.8 <- m[71:77]
Data.9 <- m[81:90]




Cor.1 <- cor(cbind(ArriveDelay,Data.1))
Cor.2 <- cor(cbind(ArriveDelay,Data.2))
Cor.3 <- cor(cbind(ArriveDelay,Data.3))
Cor.4 <- cor(cbind(ArriveDelay,Data.4))
Cor.5 <- cor(cbind(ArriveDelay,Data.5))
Cor.6 <- cor(cbind(ArriveDelay,Data.6))
Cor.7 <- cor(cbind(ArriveDelay,Data.7))
Cor.8 <- cor(cbind(ArriveDelay,Data.8))


corrplot(Cor.1, method="number")
corrplot(Cor.2, method="number")
corrplot(Cor.3, method="number")
corrplot(Cor.4, method="number")
corrplot(Cor.5, method="number")
corrplot(Cor.6, method="number")
corrplot(Cor.7, method="number")
corrplot(Cor.8, method="number")


m1<-lm(ArriveDelay~DepartureDelay,data=m)
summary(m1)

m2<-lm(ArriveDelay~.,data=m)
summary(m2)




#Principal component analysis (PCA) is a technique used to emphasize variation and 
#bring out strong patterns in a dataset. 
#It's often used to make data easy to explore and visualize.
#And, we now have all the numerical values. Let’s divide the data into test and train.
#pca.train <- new_my_data[1:nrow(new_my_data),]
#pca.test <- new_my_data[-(1:nrow(train)),]
n=0.3*nrow(new_my_data)
test.index=sample(1:nrow(new_my_data),n)
pca.train=new_my_data[-test.index,]
pca.test=new_my_data[test.index,]



#pca.train=new_my_data[1:8000,]
#pca.test=new_my_data[8001:10000,]

#To identify the zero-variance column, we can use which as follows to get the 
#variable name.

which(apply(pca.train, 2, var)==0)
which(apply(pca.test, 2, var)==0)

#And to remove zero variance columns from the dataset, you can use the 
#same apply expression, setting variance not equal to zero.

pca.train <-(pca.train[ , apply(pca.train, 2, var) != 0])
pca.test <-pca.test[ , apply(pca.test, 2, var) != 0]


#omit  variables to match the test and training set
#pca.train <- pca.train %>% 
 # select(-Hour2011-09-06 20:00:00,)

#The prcomp() function provides the facility to compute standard deviation of 
#each principal component. sdev refers to the standard deviation of principal components.
prin_comp <- prcomp(new_my_data, scale. = T)

names(prin_comp)

# center and scale refers to respective mean and standard deviation
#of the variables that are used for normalization prior to implementing PCA
#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale
#The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal
#component loading vector. This is the most important measure we should be interested in.
prin_comp$rotation

#Let’s look at first 4 principal components and first 20 rows.
 prin_comp$rotation[1:20,1:2]



dim(prin_comp$x)

#Let’s plot the resultant principal components. However it is too big
#biplot(prin_comp ,scale = 0)


#biplot(prin_comp, expand=10, xlim=c(-0.30, 0.0), ylim=c(-0.1, 0.1))

#We aim to find the components which explain the maximum variance. 
#This is because, we want to retain as much 
#information as possible using these components. So, higher 
#is the explained variance, higher will be the information contained in those components.
#compute standard deviation of each principal component
 std_dev <- prin_comp$sdev

#compute variance
 pr_var <- std_dev^2
 

#check variance of first 10 components
 pr_var[1:10]

 
 #This shows that first principal component explains 2.7% variance. Second component 
 #explains 2.22% variance. Third component explains 1.4% variance and so on.
 #proportion of variance explained
 prop_varex <- pr_var/sum(pr_var)
 prop_varex[1:20]
 
 # A scree plot is used to access components or factors which 
 #explains the most of variability in the data. It represents values in descending order.
 #scree plot
 plot(prop_varex, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        type = "b")

 #cumulative scree plot
 plot(cumsum(prop_varex), xlab = "Principal Component",
        ylab = "Cumulative Proportion of Variance Explained",
        type = "b")
 
 


 #This plot shows that 150 components results in variance close to ~ 99%. Therefore, 
 #in this case, we’ll select number of components as 150 [PC1 to PC150] and proceed to
 #the modeling stage. This completes the steps to implement PCA on train data. For modeling,
 #we’ll use these 30 components as predictor variables and follow the normal procedures.
 
  #add a training set with principal components
 train.data <- data.frame(ArriveLoad = pca.train$ArriveLoad, prin_comp$x)
 
 #we are interested in first 150 PCAs
 train.data <- train.data[,1:30]
 

 #install.packages("rpart")
 library(rpart)
 rpart.model <- rpart(ArriveDelay ~ .,data = new_my_data, method = "anova")
 rpart.model
 

 printcp(rpart.model) # display the results 
 plotcp(rpart.model) # visualize cross-validation results 
 summary(rpart.model) # detailed summary of splits
 
 
 #transform test into PCA
 test.data <- predict(prin_comp, newdata = pca.test)
 test.data <- as.data.frame(test.data)
 
 #select the first 30 components
 test.data <- test.data[,1:30]
 
 #make prediction on test data
 rpart.prediction <- predict(rpart.model, test.data,  interval='prediction') 
 rpart.prediction

 library(rpart.plot)
 rpart.plot(rpart.model)
 #i need to look into more about rpart.plot
 # load ggplot2
 library(ggplot2)
 
 # create data frame with scores
 scores = as.data.frame(prin_comp$x)
 
 # plot of observations 
 #This data will take sometime if we load it more
 ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
   geom_hline(yintercept = 0, colour = "gray65") +
   geom_vline(xintercept = 0, colour = "gray65") +
   geom_text(colour = "tomato", alpha = 0.8, size = 4) +
   ggtitle("PCA plot of Translink")
 
 
 # function to create a circlehat arrows are scaled to represent the loadings. To make inference 
 #from image above, focus on the extreme ends (top, bottom, left, right) of this graph.
 circle <- function(center = c(0, 0), npoints = 100) {
   r = 1
   tt = seq(0, 2 * pi, length = npoints)
   xx = center[1] + r * cos(tt)
   yy = center[1] + r * sin(tt)
   return(data.frame(x = xx, y = yy))
 }
 corcir = circle(c(0, 0), npoints = 100)
 
 # create data frame with correlations between variables and PCs
 correlations = as.data.frame(cor(new_my_data, prin_comp$x))
 
 x = length(correlations)
 
 X <- rep(c(0), times =  x )
 
 # data frame with arrows coordinates
 arrows = data.frame(x1 =  X, y1 = X, x2 = correlations$PC1, 
                     y2 = correlations$PC2)
 
 # geom_path will do open circles
 ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
   geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
   geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
   geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                              colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs", 
                                                                                                                            y = "pc2 axis") + ggtitle("Circle of correlations")
 
 
 cor(new_my_data,method="pearson")
 dse <- as.matrix(dse)
 m <- as.data.frame(dse)
 
m1<-lm(ArriveDelay~.,data=new_my_data)
 summary(m1)

str(new_my_data) 

 
 
 # Logistic Indicator
 data_logit <- m %>%
   mutate(Arrived = ifelse(ArriveDelay < 0, 0, 1)) %>%
   mutate(Delayed = ifelse(ArriveDelay > 0, 0, 1))

 compensated <- data_logit$Delayed
 bus_path <- m[-50]
 str(bus_path)
 
 library(glmnet)
 #running glmnet having problems with binomail but poisson is working
 model2 <- glmnet(as.matrix(bus_path),y=as.matrix(compensated), family="binomial")
 summary(model2)
 
 coef<-predict(model2, type="coefficients")
 coef
 lasso <- cv.glmnet(as.matrix(bus_path),as.matrix(compensated), family="binomial") 
 lasso_coef<-predict(lasso, type="coefficients")
 plot(model2, label = TRUE)
 plot(lasso, label =T )
 plot(coef, label=T)
 plot(lasso_coef, label=T)
 
 

 #running pairs function 
 pairs(bus_path)
 
 #histogram
 hist(bus_path$ArriveDelay , breaks = 100)
 
 dse = within(new_my_data, {
   Arrived = ifelse(ArriveDelay < 0, 0, 1)
   Delayed = ifelse(ArriveDelay > 0, 0, 1)
 })
 
 na.omit(dse)
 dse <- as.matrix(dse)
 m <- as.data.frame(dse)
 library(glmnet)
 model2 <- glm(Delayed~.,data=m, family=binomial())

Part.2 <-  filter.integers.numerics %>% 
  select(StopSeqNo, ArriveLoad, LeaveLoad, Ons, ArriveLoadCompensated, 
         OnsLoadCompensated, OffsLoadCompensated, Pattern, 
         WCLiftActivatedN, VehicleAge, VehicleCapacity, Conditions, Wind.Speed, 
         Visibility, Humidity, Temp, DwellTime, DepartureDelay) 

 
new_my_data.1 <- dummy.data.frame(Part.2, names = c("Pattern", "Line","TimingPoint",
                                                                    "WCLiftActivated","ActualLeaveTime","ScheduledLeaveTime",
                                                                    "ActualArriveTime","ScheduledArriveTime", "VehicleNo","DayType",
                                                                    "BikeLoaded","BikeUnloaded","VehicleDescription","VehicleClass",
                                                                    "VehicleFuelTyle","VehicleLength","VehicleCapacity","VehicleSittingCapacity","Conditions",
                                                                    "StopName","Hour"))
str(new_my_data.1)

which(apply(new_my_data.1, 2, var)==0)
 

dse = within(new_my_data.1, {
  Arrived = ifelse(ArriveDelay < 0, 0, 1)
  Delayed = ifelse(ArriveDelay > 0, 0, 1)
})

na.omit(dse)
dse <- as.matrix(dse)
m <- as.data.frame(dse)
model4 <- glm(Delayed~.,data=m, family=binomial()) 


# Logistic Indicator
data_logit <- new_my_data.1 %>%
  mutate(Delayed = ifelse(ArriveDelay < 0, 0, 1)) %>%
  mutate(Early = ifelse(ArriveDelay > 0, 0, 1))

compensated <- data_logit$Early
bus_path <- new_my_data.1[-24]
str(bus_path)

#running glmnet having problems with binomail but poisson is working
model2 <- glmnet(as.matrix(bus_path),y=as.matrix(compensated), family="binomial")
summary(model2)

coef<-predict(model2, type="coefficients")
coef
lasso <- cv.glmnet(as.matrix(bus_path),as.matrix(compensated), family="binomial") 

lasso_coef<-predict(lasso, type="coefficients")
plot(model2, label = TRUE)
plot(lasso, label =T )
plot(coef, label=T)
plot(lasso_coef, label=T)

m2<-lm(ArriveDelay~.,data=new_my_data.1)
summary(m1)

dse <- as.matrix(new_my_data.1)
m <- as.data.frame(dse)



#######################################################################################
######################################################################################



x_numeric <- m
x_numeric <- x_numeric[complete.cases(x_numeric),]
#------------------ build model -------------------------------

arrival_model <- function(trainData, target){
  #set.seed(120)
  my_lm <- lm(ArriveDelay ~ ., data=trainData)
  return(my_lm)  
}

#------------------ run K-fold -----------------------------
error <- NULL
rmsd  <- NULL
nRuns <- 100
kSize <- 1000

# perform k-fold
for(i in 1:nRuns)
{
  randomSample <- sample(1:nrow(x_numeric), kSize, replace = FALSE)
  x.train <- x_numeric[randomSample, -1] # remove ID
  x.test <- x_numeric[-randomSample, -1] # remove ID
  
  mod <- arrival_model(x.train, ArriveDelay)
  pred <- predict(mod, newdata = x.test)
  y.pred <- pred
  y.true <- x.test$ArriveDelay
  rmsd[i] <- sqrt(sum((y.pred - y.true)^2)/length(y.true))
  isCorrect <- ifelse(abs(y.pred - y.true) < 1, 1, 0) # Check if prediction is off by more than 1 minute
  error[i] <- sum(isCorrect)/length(isCorrect)  # fraction of the time that 1 minute standard is met
}

# plot Root Mean Square Deviation over iterations
plot(cumsum(rmsd)/seq_along(rmsd), type = "l")
# plot error larger than 1 minute over iterations
plot(cumsum(error)/seq_along(error), type = "l")
# plot percent of predictions within 1 minute at each iteration
plot(error*100)

#---------------- Bootstrapping ----------------------------
# quantify variance in your model

# fit model
mod <- arrival_model(x_numeric[,-1], ArriveDelay)
# set params
error <- NULL
nRuns <- 50
sampleSize <- 50

for(i in 1:nRuns)
{
  subsetDat <- dplyr::sample_n(x_numeric, sampleSize)
  pred <- predict(mod, newdata = subsetDat[,-1], type = "response")
  y.pred <- ifelse(pred > 0.5, 1, 0)
  y.true <- subsetDat$ArriveDelay
  isCorrect <- ifelse(y.pred == y.true, 1, 0)
  error[i] <- sum(isCorrect)/length(isCorrect)
}

mse <- mean(error^2)
mse


