'''

Given five positive integers, find the minimum and maximum values that can be calculated by summing exactly four of the five integers. Then print the respective minimum and maximum values as a single line of two space-separated long integers.

Input Format

A single line of five space-separated integers.

Constraints

Each integer is in the inclusive range .
Output Format


#!/bin/python3

import sys

def miniMaxSum(arr):
    # Complete this function

if __name__ == "__main__":
    arr = list(map(int, input().strip().split(' ')))
    miniMaxSum(arr)

'''

import sys

arr = list(map(int, input().strip().split(' ')))

# sort out array
arr.sort()
# legnth of array
n = len(arr)

# print %i for the sum of array on Mini-Max Sum
print("%i %i"%(sum(arr[0:4]),sum(arr[n-4:n])))


