
'''
The ratio of boys to girls for babies born in Russia is . If there is  child born per birth, what proportion of Russian families with exactly  children will have at least  boys?

Write a program to compute the answer using the above parameters. Then print your result, rounded to a scale of  decimal places (i.e.,  format).
'''

#!/usr/bin/python
def comb(n,r):
	x=1
	for i in range(r): x=x*(n-i)//(i+1)
	return x

N=6
P=0.8
Q=1-P
print('%.3f'%sum(comb(N,i)*P**i*Q**(N-i) for i in range(3,N+1)))
print('%.3f'%sum(comb(N,i)*P**i*Q**(N-i) for i in range(0,2)))


#############

import math
import operator as op
from functools import reduce
def ncr(n, r):
    r = min(r, n-r)
    if r == 0: return 1
    numer = reduce(op.mul, range(n, n-r, -1))
    denom = reduce(op.mul, range(1, r+1))
    return numer//denom

def binom(total, desired, prob_success):
	return ncr(total, desired) * math.pow(prob_success, desired) * math.pow(1-prob_success, total - desired)

e = list(map(float, input().split()))
prob_boy = e[0]/(e[0]+e[1])

print(round(sum([binom(6, i, prob_boy) for i in range(3, 6+1)]), 3))




####################


def fact(n):
    return 1 if n == 0 else n*fact(n-1)

def comb(n, x):
    return fact(n) / (fact(x) * fact(n-x))

def b(x, n, p):
    return comb(n, x) * p**x * (1-p)**(n-x)

l, r = list(map(float, input().split(" ")))
odds = l / r
print(round(sum([b(i, 6, odds / (1 + odds)) for i in range(3, 7)]), 3))