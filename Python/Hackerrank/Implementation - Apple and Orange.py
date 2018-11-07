#!/bin/python3
'''
Sam's house has an apple tree and an orange tree that yield an abundance of fruit. Sam’s two children, Larry and Rob, decide to play a game in which they each climb a tree and throw fruit at their (Sam’s) house. Each fruit that lands on the house scores one point for the one who threw it. Larry climbs the tree on the left (the apple), and Rob climbs the one on the right (the orange).

We’ll use the following diagram to describe the challenge:

Apple and orange(2).png

For simplicity, we’ll assume all of the landmarks are on a number line. Larry climbs the apple tree at point , and Rob climbs the orange tree at point . Sam’s house stands between points  and . Values increase from left to right.

You will be given a list of distances the fruits are thrown. Negative distances indicate travel left and positive distances, travel right. Your task will be to calculate the scores for Larry and Rob and report them each on a separate line.

Input Format

 space-separated integers

 and , left and right sides of Sam’s house

 space-separated integers

 and , Larry’s and Rob’s positions in the trees

 space-separated integers

 and , number of apples and oranges thrown

 space-separated integers

distances  that each apple falls from 

 space-separated integers

distances  that each orange falls from 

Output Format

2 space-separated integers on a line: Larry’s score followed by Rob’s score.

Sample Input 0

7 11
5 15
3 2
-2 2 1
5 -6
Sample Output 0

1 1
Explanation 0

The first apple falls at position  5 -2 =3
The second apple falls at position   5 +2 =7
The third apple falls at position   5 + 1 = 6
The first orange falls at position  15 + 5 = 20
The second orange falls at position  15 - 6 = 9

Only one fruit (the second apple) falls within the region between  and , so Larry’s score is . 
Only the second orange falls within the region between  and , so Rob’s score is .
'''
import sys

s,t = input().strip().split(' ')
s,t = [int(s),int(t)]
a,b = input().strip().split(' ')
a,b = [int(a),int(b)]
m,n = input().strip().split(' ')
m,n = [int(m),int(n)]
apple = [int(apple_temp) for apple_temp in input().strip().split(' ')]
orange = [int(orange_temp) for orange_temp in input().strip().split(' ')]



apple_count = 0
orange_count = 0

for apples in apple:
    if(apples + a >= s and apples + a <= t):
        apple_count += 1
 

for oranges in orange:
    if(oranges + b <= t and oranges + b >= s):
        orange_count += 1
    
            
print(apple_count)
print(orange_count)