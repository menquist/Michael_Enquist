'''
There are two kangaroos on a number line ready to jump in the positive direction (i.e, toward ). Each kangaroo takes the same amount of time to make a jump, regardless of distance. That is, if kangaroo one jumps 3 meters and kangaroo two jumps 5 meters, they each do it in one second, for example.

Given the starting locations and jump distances for each kangaroo, determine if and when they will land at the same location at the same time.

Input Format

4 space-separated integers 
: starting locations  and meters per jump  for kangaroos  and 


Output Format

Print YES if they can land on the same location at the same time. Otherwise, print NO.

Sample Input 0

0 3 4 2
Sample Output 0

YES
Explanation 0

The two kangaroos jump through the following sequence of locations: 
image
The kangaroos meet after 4 jumps.

Sample Input 1

0 2 5 3
Sample Output 1

NO
Explanation 1

Kangaroo 2 is travelling faster than kangaroo 1, so they will never meet.
'''

#!/bin/python3

import sys

x1, v1, x2, v2 = map(int, input().split())

# Note: 0 <= x1 < x2 <= 10000
# x1 + k.v1 = x2 + k.v2




diff = x2 - x1
a = 0
for i in range(0,diff):
    x1 += v1
    x2 += v2
    if(x1 == x2):
        print("YES")
        a = 1
        break
        
if a != 1:
    print("NO")


