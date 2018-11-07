'''

Lily has a chocolate bar consisting of a row of squares where each square has an integer written on it. She wants to share it with Ron for his birthday, which falls on month and day . Lily only wants to give Ron a piece of chocolate if it contains consecutive squares whose integers sum to .
Given , , and the sequence of integers written on each square of Lily's chocolate bar, how many different ways can Lily break off a piece of chocolate to give to Ron?
For example, if , and the chocolate bar contains rows of squares with the integers
written on them from left to right, the following diagram shows two ways to break off a piece:
                             Input Format
The first line contains an integer denoting (the number of squares in the chocolate bar). The second line contains space-separated integers describing the respective values of (the numbers written on each consecutive square of chocolate).
The third line contains two space-separated integers describing the respective values of day) and (Ron's birth month).
Constraints
, where ( )
Output Format
(Ron's birth
                                                  Print an integer denoting the total number of ways that Lily can give a piece of chocolate to Ron.
Sample Input 0
5 12132 32
Sample Output 0
2
Explanation 0
This sample is already explained in the problem statement.
Sample Input 1
6 111111 32
   
Sample Output 1
 0
Explanation 1
Lily only wants to give Ron consecutive squares of chocolate whose integers sum to . There are no possible pieces satisfying these constraints:
       Thus, we print as our answer.
Sample Input 2
1
4 41
Sample Output 2
1
Explanation 2
Lily only wants to give Ron
square of chocolate in the bar satisfies this constraint, we print as our answer.
. Because the only
   square of chocolate with an integer value of
      


import sys

def solve(n, s, d, m):
    # Complete this function

n = int(input().strip())
s = list(map(int, input().strip().split(' ')))
d, m = input().strip().split(' ')
d, m = [int(d), int(m)]
result = solve(n, s, d, m)
print(result)
'''

import sys

def solve(n, s, d, m):
    return sum([1 for i in range(n-m+1) if sum(s[i:i+m]) == d])

n = int(input().strip())
s = list(map(int, input().strip().split(' ')))
d, m = input().strip().split(' ')
d, m = [int(d), int(m)]
result = solve(n, s, d, m)
print(result)