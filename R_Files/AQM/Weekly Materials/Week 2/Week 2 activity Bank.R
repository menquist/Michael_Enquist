Bank <- read.table("Bank.txt", header = T)
dim(Bank)
str(Bank)

model <- lm(SALBEG ~ SEX + JOBCAT, data = Bank)
summary(model)
anova(model)
#We are assuming that sex and job categories have an effect on