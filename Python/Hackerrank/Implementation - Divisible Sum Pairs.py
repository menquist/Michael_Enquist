
'''
You are given an array of  integers, , and a positive integer, . Find and print the number of  pairs where  and  +  is divisible by .

Input Format

The first line contains  space-separated integers,  and . 
The second line contains  space-separated integers describing the values of .
'''

#!/bin/python3

import sys

n,k = input().strip().split(' ')
n,k = [int(n),int(k)]
a = list(map(int,input().strip().split(' ')))
res = 0
for i in range(len(a)):
    for j in range(i+1, len(a)):
        if (a[i] + a[j]) % k == 0:
            res += 1
print(res)

