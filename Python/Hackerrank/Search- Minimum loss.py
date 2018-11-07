'''

Lauren has a chart of distinct projected prices for a house over the next N  years, where the price of the house in 
the ith year is pi . She wants to purchase and resell the house at a minimal loss according to the following rules:

The house cannot be sold at a price greater than or equal to the price it was purchased at (i.e., it must be resold at a loss).
The house cannot be resold within the same year it was purchased.
Find and print the minimum amount of money Lauren must lose if she buys the house and resells it within the next  years.

Note: It's guaranteed that a valid answer exists.

Input Format

The first line contains an integer,n , denoting the number of years of house data. 
The second line contains   n space-separated long integers describing the respective values of p1,p2,...pn .

Constraints



All the prices are distinct.
It's guaranteed that a valid answer exists.
Subtasks

 for  of the maximum score.
Output Format

Print a single integer denoting the minimum amount of money Lauren must lose if she buys and
resells the house within the next  years.

Sample Input 0

3
5 10 3
Sample Output 0

2
Explanation 0

Lauren buys the house in year  at price  and sells it in year  at  for a minimal loss of .

Sample Input 1

5
20 7 8 2 5
Sample Output 1

2
Explanation 1

Lauren buys the house in year 2  at price p2= 7  and sells it in year 5 at p5 =5  for a minimal loss of 7-5=2 .

#!/bin/python3

import sys

def minimumLoss(price):
    # Complete this function

if __name__ == "__main__":
    n = int(input().strip())
    price = list(map(int, input().strip().split(' ')))
    result = minimumLoss(price)
    print(result)
'''


n = int(input())
numbers = list(map(int, input().split()))
d = []
#Make an iterator that computes the function using arguments from
#each of the iterables.  Stops when the shortest iterable is exhausted

for i in range(len(numbers)):
        d.append((numbers[i], i+1))
def getKey(item):
    return item[0]

d.sort(key=getKey, reverse=True)

min_loss = 10000000000000000
for i in range(n-1):
    # subtract row(2,1) - row(1,1) and at second row -> row(2,2) - row(1,1)
    if d[i][1] < d[i+1][1]:
        if (d[i][0] - d[i+1][0]) < min_loss:
            min_loss = d[i][0] - d[i+1][0]

print(min_loss)
