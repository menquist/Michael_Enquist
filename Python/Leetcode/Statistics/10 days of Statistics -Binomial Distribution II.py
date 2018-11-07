
'''
Objective 
In this challenge, we go further with binomial distributions. We recommend reviewing the previous challenge's Tutorial before attempting this problem.

Task 
A manufacturer of metal pistons finds that, on average,  of the pistons they manufacture are rejected because they are incorrectly sized. What is the probability that a batch of  pistons will contain:

No more than  rejects?
At least  rejects?
Input Format

A single line containing the following values (denoting the respective percentage of defective pistons and the size of the current batch of pistons):

12 10
If you do not wish to read this information from stdin, you can hard-code it into your program.

Output Format

Print the answer to each question on its own line:

The first line should contain the probability that a batch of  pistons will contain no more than  rejects.
The second line should contain the probability that a batch of  pistons will contain at least  rejects.
Round both of your answers to a scale of  decimal places (i.e.,  format).
'''

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
prob_reject = e[0]/100

print(round(sum([binom(int(e[1]), i, prob_reject) for i in range(0, 2+1)]), 3))
print(round(sum([binom(int(e[1]), i, prob_reject) for i in range(2, 10+1)]), 3))