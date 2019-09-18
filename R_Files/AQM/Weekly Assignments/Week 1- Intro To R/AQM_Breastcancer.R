setwd("~/github/Enquist_Mike/Enquist_Mike/AQM Assignments")


f1 <- function(pal){
  x<- read.csv(pal)
  x<- as.data.frame(x)
  return(x)
}


BreastCancer <- f1("BreastCancer.csv")[-1]
str(BreastCancer)

BreastCancer <- BreastCancer[2:11]
BreastCancer <- na.omit(BreastCancer)


BreastCancer$Class <- factor(BreastCancer$Class, levels=c(0,1), labels = c("benign","malignant"))
model2 <- glm(Class~.,data=BreastCancer, family=binomial())
summary(model2)
#Likilyhood of the model?

# Logistic Indicator

pred <- predict(model2, type = "response")

logit.pred <- factor(pred > .5, levels = c(F,T),labels = c("benign","malignant"))

logit.perf <- table(BreastCancer$Class,logit.pred, dnn = c("Actual","Predicted"))

summary(logit.perf)
str(logit.perf)


Class<- as.matrix(BreastCancer)
BreastCancer <- as.matrix(BreastCancer)

library(glmnet)
model2 <- glm(Class~.,data=BreastCancer, family=binomial())
summary(model2)






