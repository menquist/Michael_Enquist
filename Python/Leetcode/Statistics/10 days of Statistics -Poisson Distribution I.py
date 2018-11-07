
'''
Objective 
In this challenge, we learn about Poisson distributions. Check out the Tutorial tab for learning materials!

Task 
A random variable, , follows Poisson distribution with mean of . Find the probability with which the random variable  is equal to .

Input Format

The first line contains 's mean. The second line contains the value we want the probability for:

'''
import math

avg = float(input())
actual = float(input())

def poisson(actual, avg):
	return math.pow(avg, actual) * math.pow(math.e, -avg) / math.factorial(actual)


print(poisson(actual, avg))
