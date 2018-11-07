'''
Maria plays  games of college basketball in a season. Because she wants to go pro, she tracks her points scored per game sequentially in an array defined as . After each game , she checks to see if score  breaks her record for most or least points scored so far during that season.

Given Maria's array of  for a season of  games, find and print the number of times she breaks her record for most and least points scored during the season.

Note: Assume her records for most and least points at the start of the season are the number of points scored during the first game of the season.
import sys

def breakingRecords(score):
    # Complete this function

if __name__ == "__main__":
    n = int(input().strip())
    score = list(map(int, input().strip().split(' ')))
    result = breakingRecords(score)
    print (" ".join(map(str, result)))

Input (stdin)
9
10 5 20 20 4 5 2 25 1
Your Output (stdout)
2 4
Expected Output
2 4

'''

#!/bin/python3


x,n = input(), list(map(int, input().split()))
xmax = xmin = n[0]
xcmax = xcmin = 0

# highest and lowest scores
for ni in n:
    if ni < xmin:
        xcmin += 1
        xmin = ni 
    elif ni > xmax:
        xcmax += 1
        xmax = ni 

print(xcmax, xcmin)


