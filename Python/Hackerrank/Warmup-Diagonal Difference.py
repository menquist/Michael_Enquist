'''

Given a square matrix, calculate the absolute difference between the sums of its diagonals.

Input Format

The first line contains a single integer,  denoting the number of rows and columns in the matrix . 
The next  lines denote the matrix 's rows, with each line containing  space-separated integers describing the columns.

Constraints


Output Format

Print the absolute difference between the sums of the matrix's two diagonals as a single integer.


import sys

 

if __name__ == "__main__":
    n = int(input().strip())
    a = []
    for a_i in range(n):
       a_t = [int(a_temp) for a_temp in input().strip().split(' ')]
       a.append(a_t)
    result = diagonalDifference(a)
    print(result)
'''

import sys


n = int(input().strip())
a = []
for a_i in range(n):
    a_temp = list(map(int,input().strip().split(' ')))
    a.append(a_temp)
sum1 = 0
sum2 = 0


#Sum across the primary diagonal:
for i in range(len(a)):
    sum1 += a[i][i]
    sum2 += a[i][len(a[0])-1-i]
print(abs(sum1 - sum2))    
    


