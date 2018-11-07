'''
You will be given two arrays of integers. You will be asked to determine all integers that satisfy the following two conditions:

The elements of the first array are all factors of the integer being considered
The integer being considered is a factor of all elements of the second array
These numbers are referred to as being between the two arrays. You must determine how many such numbers exist.

Input Format

The first line contains two space-separated integers describing the respective values of , the number of elements in array , and , the number of elements in array . 
The second line contains  distinct space-separated integers describing . 
The third line contains  distinct space-separated integers describing .

Constraints




Output Format

Print the number of integers that are considered to be between  and .

Sample Input

2 3
2 4
16 32 96
Sample Output

3
Explanation

2 and 4 divide evenly into 4, 8, 12 and 16. 
4, 8 and 16 divide evenly into 16, 32, 96.

4, 8 and 16 are the only three numbers for which each element of A is a factor and each is a factor of all elements of B
'''


#!/bin/python3

import sys


n, m = input().strip().split(' ')
n, m = [int(n), int(m)]
a = list(map(int, input().strip().split(' ')))
b = list(map(int, input().strip().split(' ')))

a.sort()
b.sort()
output = 0
for q in range(b[0] +1):
    if q >= a[-1]:
        for t in range(n):
            if q % a[t] != 0:
                break
            if t == n -1:
                for g in range(m):
                    if b[g] % q != 0:
                        break
                    if g == m -1:
                        output += 1
      
print(output)

