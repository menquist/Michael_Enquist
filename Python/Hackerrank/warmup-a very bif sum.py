'''
Calculate and print the sum of the elements in an array, keeping in mind that some of those integers may be quite large.

Input Format

The first line of the input consists of an integer . 
The next line contains  space-separated integers contained in the array.

Output Format

Print the integer sum of the elements in the array.
'''
#!/bin/python3
import sys


n = int(input().strip())
arr = list(map(int,input().strip().split(' ')))
sum=0
for i in range(n):
    sum = sum + arr[i]
print(sum)

