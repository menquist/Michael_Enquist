'''
Given an array of integers, calculate which fraction of its elements are positive, which fraction of its elements are negative, and which fraction of its elements are zeroes, respectively. Print the decimal value of each fraction on a new line.

Note: This challenge introduces precision problems. The test cases are scaled to six decimal places, though answers with absolute error of up to  are acceptable.

Input Format

The first line contains an integer, , denoting the size of the array. 
The second line contains  space-separated integers describing an array of numbers .

Output Format

You must print the following  lines:

A decimal representing of the fraction of positive numbers in the array compared to its size.
A decimal representing of the fraction of negative numbers in the array compared to its size.
A decimal representing of the fraction of zeroes in the array compared to its size.

'''



    #!/bin/python3

import sys
from decimal import *

n = int(input().strip())
arr = [int(arr_temp) for arr_temp in input().strip().split(' ')]

numOfPositives = 0
numOfNegatives = 0
numOfZeros = 0

for integer in arr:
    if (integer > 0):
        numOfPositives = numOfPositives + 1

    elif (integer < 0):
        numOfNegatives = numOfNegatives + 1

    else:
        numOfZeros = numOfZeros + 1
        
fractionOfPositives = Decimal(numOfPositives) / Decimal(n)
fractionOfPositivesWithPrecision = format(fractionOfPositives, '.6f')
print(fractionOfPositivesWithPrecision)

fractionOfNegatives = Decimal(numOfNegatives) / Decimal(n)
fractionOfNegativesWithPrecision = format(fractionOfNegatives, '.6f')
print(fractionOfNegativesWithPrecision)

fractionOfZeros = Decimal(numOfZeros) / Decimal(n)
fractionOfZerosWithPrecision = format(fractionOfZeros, '.6f')