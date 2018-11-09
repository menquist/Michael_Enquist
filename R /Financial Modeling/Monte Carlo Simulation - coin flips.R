set.seed(666) # what does this do? Try changin it and re-running the code

#basically set.seed() function will help to reuse the same set of random variables 
# which we may need in future to again evaluate particular task again with same random varibales

we just need to declare it before using any random numbers generating function.

runs <- 100000 # number of random draws
siteA <- rbeta(runs, 20, 200) # random draws from siteA dist
siteB <- rbeta(runs, 40, 300) # random draws from siteB dist
pval.sim <- sum(siteA > siteB)/runs # Identify the p-value of siteA > siteB

# plot the distributions
curve(dbeta(x, 20, 100), col = "red", lwd=2, xlim = c(0, 0.5), 
      ylab="Beta Density", main="siteA (red) vs siteB (blue) Beta Distributions")
curve(dbeta(x, 40, 115), add=TRUE, col="blue", lwd=2)

ggplot(data.frame(diff = siteA/siteB), aes(diff, ..density..)) +
  geom_histogram(stat="bin", binwidth=0.1, fill = "skyblue", colour = "black") +
  xlab("Runs of 7") +
  theme_bw()

#FREQUENCY AND COIN FLIPS
# setting the seed ensures that my "random" results are repeatable
# for others to see when re-running my experiment. Change seed to
# see another variation of reality
set.seed(99)

# create a sequence of coin tosses. Increase/decrease the number of
# flips to see how well it converges to the probability of 0.5.

#--------Choose number of flips-------
N <- 5000
#-------------------------------------

# simulate number number of coin flips using a uniform distribution,
# then calculate the cumulative mean. Try to understand what's going
# on here.
coinTosses <- data.frame(index = 1:N, draw_head = round(runif(N))) %>%
  mutate(cumMean = cumsum(draw_head)/seq_along(draw_head))

# plot the cumulative mean over the index value
plot <- ggplot(coinTosses, aes(y=cumMean, x=index)) + 
  geom_line(colour="blue") + theme_bw() +
  geom_hline(y=0.5, colour="red", linetype="dashed") +
  ggtitle("Cumulative Mean of N Coin Tosses") +
  theme_bw()
print(plot)


# How Many Runs of 6 heads can I expect after 1000 flips?
# initiate empty vector
runs_of_6 <- NULL

# count the number of times a sequence of 6 heads pops up in a 1000 flip
# sample over 10,000 tries.
for(i in 1:10000) {
  # what does the function rle do?
  runs_of_6[i] <- sum(rle(sample(c(-1, 1), 1000, TRUE))$lengths == 7)
}

# plot the histogram
ggplot(data.frame(runs = runs_of_6), aes(runs)) +
  geom_histogram(stat='bin', colour = "black", fill = "orange", binwidth=1) +
  xlab("Runs of 7 Heads") +
  theme_bw()