
#This data is a collection of information about the number of male and female months in an area of South Wales

#There are 16 Colonies of moths and we are interested in estimating the True mean proportion of Female Moths in population

#The function of the Moths' genders is binomial where each moth is either Female or Not Female

#These are the numbers of females and males in each of the 16 colonies of Moths
Females <-c(18,31,34,33,27,33,28,23,33,12,19,25,14,4,22,7)
Males<-c(11,22,27,29,24,29,25,26,38,14,23,31,20,6,34,12)
Y<-Females

N<-Females+Males 

# This is a vector containing the proportion of Female Moths for each colony
Y/N
# This is the total proportion of female moths across all colonies 
sum(Y)/sum(N)

# Formula for calculating the loglikelyhood of a given proportion(p) of Female Moths
logL<-function(p) sum(dbinom(Females,N,p,log=T))

#Find the Proportion with the Maximum Likelyhood usingthe optimize function: It gives approx. 50%
optimize(logL,lower=0,upper=1,maximum=TRUE)

#Interactive Plot to illustrate that 


########################################################################
require(manipulate)
p.seq<-seq(0.01,0.99,0.01) # Range of Probabilities with minimum of 0 and max of 1
manipulate(
  plot(p.seq,sapply(p.seq,logL),type="l",ylab = "Log Likelyhood of Proportion(Moths)", xlab= "Proportion of Female Moths")+abline(v=Probability, col="red"),
  Probability=slider(0.0,1.0,step = 0.1,initial = 0.5) # Slider T oshow a Line at the selected probability
)
    
