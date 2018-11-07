'''
Hackerland is a one-dimensional city with  houses, where each house  is located at some  on the -axis. The Mayor wants to install radio transmitters on the roofs of the city's houses. Each transmitter has a range, , meaning it can transmit a signal to all houses  units of distance away.

Given a map of Hackerland and the value of , can you find and print the minimum number of transmitters needed to cover every house in the city? (Every house must be covered by at least one transmitter) Each transmitter must be installed on top of an existing house.

Input Format

The first line contains two space-separated integers describing the respective values of  (the number of houses in Hackerland) and  (the range of each transmitter). 
The second line contains  space-separated integers describing the respective locations of each house (i.e., ).

Constraints

There may be more than one house at the same location.
Subtasks

 for  of the maximum score.
Output Format

Print a single integer denoting the minimum number of transmitters needed to cover all the houses.

Sample Input 0

5 1
1 2 3 4 5
Sample Output 0

2
Explanation 0

The diagram below depicts our map of Hackerland:

k-nearest(2).png

We can cover the entire city by installing transmitters on houses at locations  and . Thus, we print  on a new line.

Sample Input 1

8 2
7 2 4 6 5 9 12 11 
Sample Output 1

3
Explanation 1

The diagram below depicts our map of Hackerland:

k-nearest2(2).png

We can cover the entire city by installing transmitters on houses at locations , , and . Thus, we print  on a new line.
'''

#!/bin/python3

import sys


n,k = input().strip().split(' ')
n,k = [int(n),int(k)]
x = list(map(int,input().strip().split(' ')))

# sort the array of houses by their location
x.sort()

# a function to determine on which house to put a transmitter
# ideally, it should be the furthest house possible
def furthest_to_place(x, tr_range, i):
    next_i = i
    current = x[next_i]
    while next_i < len(x) - 1 and current <= x[i] + tr_range:
        next_i += 1
        current = x[next_i]
    if current == x[i] + tr_range:
        return x[next_i]
    else:
        return x[next_i - 1] 
        


# calculate the range of the transmittor
initial = furthest_to_place(x, k, 0)
tr_range = initial + k

# starting from the beginning of the array, check how many transmitters can cover the entire city
count = 1

for i in range(1, n):
    if x[i] > tr_range:
        count += 1
        if i < n - 1:
            tr_range = furthest_to_place(x,k,i) + k

        
print(count)