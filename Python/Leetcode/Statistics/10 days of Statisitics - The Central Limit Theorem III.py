'''
You have a sample of 100 values from a population with mean 500  and with standard deviation  100.
Compute the interval that covers the middle 95%  of the distribution of the sample mean; in other words, compute  and  such that . Use the value of . Note that  is the z-score.
'''

# Enter your code here. Read input from STDIN. Print output to STDOUT
import math

average = 500
std_dev = 80

print(500 - 1.96 * (std_dev / math.sqrt(100)))
print(500 + 1.96 * (std_dev / math.sqrt(100)))