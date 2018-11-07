'''

Consider an array of integers, . We define the absolute difference between two elements, and  (where ), 
to be the absolute value of .

Given an array of  integers, find and print the minimum absolute difference between any two elements in the array.

Input Format

The first line contains a single integer denoting  (the number of integers). 
The second line contains  space-separated integers describing the respective values of .

#!/bin/python3

import sys

def minimumAbsoluteDifference(n, arr):
    # Complete this function

if __name__ == "__main__":
    n = int(input().strip())
    arr = list(map(int, input().strip().split(' ')))
    result = minimumAbsoluteDifference(n, arr)
    print(result)
    
    
'''



n,a = input(),sorted(map(int, input().split()))
print(min(abs(x-y) for x,y in zip(a,a[1:])))




n,a = input(),sorted(map(int, input().split()))
print(min(abs(x-y) for x,y in zip(a,a[1:])))