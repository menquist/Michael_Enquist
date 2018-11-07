'''
Task 
The number of tickets purchased by each student for the University X 
vs. University Y football game follows a distribution that has a mean of 2.4
and a standard deviation of 2.

A few hours before the game starts, 100 eager students line up to 
purchase last-minute tickets. If there are only  tickets left, 
what is the probability that all 100  students will be able to purchase tickets?
'''


# Enter your code here. Read input from STDIN. Print output to STDOUT
import math

def cdf(std_dev, avg, x):
    inner = 1 + math.erf((x - avg) / (std_dev * math.sqrt(2)))
    return inner * 0.5

ans = cdf(math.sqrt(100) * 2.0, 100 * 2.4, 250)

print(round(ans, 4))
