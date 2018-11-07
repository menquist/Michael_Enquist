'''
John works at a clothing store and he's going through a pile of socks to find the number of matching pairs. More specifically, he has a pile of  loose socks where each sock  is labeled with an integer, , denoting its color. He wants to sell as many socks as possible, but his customers will only buy them in matching pairs. Two socks,  and , are a single matching pair if they have the same color ().

Given  and the color of each sock, how many pairs of socks can John sell?

Input Format

The first line contains an integer, , denoting the number of socks. 
The second line contains  space-separated integers describing the respective values of .

Constraints



Output Format

Print the total number of matching pairs of socks that John can sell.

Sample Input

9
10 20 20 10 10 30 50 10 20
Sample Output

3
Explanation

sock.png

As you can see from the figure above, we can match three pairs of socks. Thus, we print  on a new line.

'''

#!/bin/python3
import sys


#we can match three pairs of socks. Thus, we print  on a new line.
def sockMerchant(n, ar):
    d = {}
    counter = 0
    for i in range(n):
        if ar[i] not in d:
            d[ar[i]] = 1
        else:
            counter += 1
            del d[ar[i]]
    return counter

n = int(input().strip())
ar = list(map(int, input().strip().split(' ')))
result = sockMerchant(n, ar)
print(result)

