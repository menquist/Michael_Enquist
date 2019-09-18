# Input cash flows of specific projects
cashFlows1 <- c(-1000, rep(220,6))
cashFlows2 <- c(-1000, rep(0,5), 1550)
discountRate <- 0.05
 
# we created two different streams of cashflows, and a discount rate of 5%

# number of cash flows and there are 7 columns in cashFlows1.
n <- length(cashFlows1)

# Creating discountCF_1 and discountCF_2 as null object variables 
discountCF_1 <- rep(0, n)
discountCF_2 <- rep(0, n)

for(i in 1:n) {
  discountCF_1[i] <- cashFlows1[i]/(1+discountRate)^(i-1)
  discountCF_2[i] <- cashFlows2[i]/(1+discountRate)^(i-1)
  discountMat <- cbind(discountCF_1, discountCF_2)
}
#Q1 Please explain the loop below in a detailed manner. What is the loop attempting to do? What is the purpose of each variable/object? What object will have our results? Please output the object created by the loop and indicate whether or not the results are reasonable.
# As we created the two cashflows and the discount rate as an object and given variables, we created discount cashflows as well in order to create the NPV formula. The loop for(i in 1:n) is implicating for i as the interest rate and going to n -> which is the length of cashflows, to create the NPV formula. Then we combine these two cashflows by using Cbind in order to determine which cashflow has the highest value and create discountMat as an object.
summary(cashFlows1)
summary(discountMat)
str(discountMat)
dim(discountMat)
print(discountMat)
