'''
# Imagine you landed a new job as a cashier...
#
# Your quirky boss found out that you're a programmer and has a weird
# request about something they've been wondering for a long time.
#
# Write a function that, given:
#
# 1. an amount of money
# 2. a list of coin denominations
#
# computes the number of ways to make amount of money with coins of the
# available denominations.
#
# Example: for amount=4 (4c) and denominations=[1,2,3] (1c, 2c and 3c),
# your program would output 4 - the number of ways to make


In this problem you will be given a list of coin denominations and a target amount. 
Determine the number of ways the target amount can be arrived at using the denominations available.

Input Format

2 space-separated integers

 and : target amount, number of denominations


 space-separated integers

: unique coin denominations

Hints

Solve overlapping subproblems using Dynamic Programming (DP): 
You can solve this problem recursively but will not pass all the test cases without optimizing 
to eliminate the overlapping subproblems. Think of a way to store and reference previously computed solutions to avoid solving the same subproblem multiple times.
Consider the degenerate cases: 
How many ways can you make change for  cents?
How many ways can you make change for  cents if you have no coins?
If you're having trouble defining your solutions store, then think about it in terms of the base case .
The answer may be larger than a -bit integer.
Output Format

Print a long integer denoting the number of ways we can get a sum of  from the given infinite supply of  types of coins.




import sys

def getWays(n, c):
    # Complete this function

n, m = input().strip().split(' ')
n, m = [int(n), int(m)]
c = list(map(int, input().strip().split(' ')))
# Print the number of ways of making change for 'n' units using coins having the values given by 'c'
ways = getWays(n, c)
'''


#create the variables in a map integer space, and in split array.
n,m = map(int,input().split())

# create varible coins in a list format
coins = list(map(int,input().split()))
dp = [1]+[0]*n

# create permutations within coins and dp variables
for i in range(m):
    for j in range(coins[i], n+1): dp[j] += dp[j-coins[i]]
print(dp[-1]) 


