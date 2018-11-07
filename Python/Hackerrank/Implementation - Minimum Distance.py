'''
Consider an array of  integers, . The distance between two indices,  and , is denoted by .

Given , find the minimum  such that  and . In other words, find the minimum distance between any pair of equal elements in the array. If no such value exists, print .

Note:  denotes the absolute value of .

Input Format

The first line contains an integer, , denoting the size of array . 
The second line contains  space-separated integers describing the respective elements in array .

Constraints



Output Format

Print a single integer denoting the minimum  in ; if no such value exists, print .

Sample Input

6
7 1 3 4 1 7
Sample Output

3
Explanation 
Here, we have two options:

 and  are both , so .
 and  are both , so .
The answer is .

'''

import sys


n = int(input().strip())
A = list(map(int,input().strip().split(' ')))
res = sys.maxsize
dic = {}
for i in range(n):
    if A[i] not in dic:
        dic[A[i]] = i
    else:
        res = min(res, i - dic[A[i]])
        dic[A[i]] = i
print(res if res < sys.maxsize else -1)