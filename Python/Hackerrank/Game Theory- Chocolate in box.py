'''
Dexter and Debra are playing a game. They have N containers each having one or more chocolates. Containers are numbered from 1 to N, where ith container has A[i] number of chocolates.

The game goes like this. First player will choose a container and take one or more chocolates from it. Then, second player will choose a non-empty container and take one or more chocolates from it. And then they alternate turns. This process will continue, until one of the players is not able to take any chocolates (because no chocolates are left). One who is not able to take any chocolates loses the game. Note that player can choose only non-empty container.

The game between Dexter and Debra has just started, and Dexter has got the first Chance. He wants to know the number of ways to make a first move such that under optimal play, the first player always wins.

Input Format

The first line contains an integer N, i.e., number of containers. 
The second line contains N integers, i.e., number of chocolates in each of the containers separated by a single space.

Constraints

1 ≤ N ≤ 106 
1 ≤ A[i] ≤ 109

Output Format

Print the number of ways to make the first move such that under optimal play, the first player always wins. If the first player cannot win under optimal play, print 0.


"In normal play, the winning strategy is to finish every move with a nim-sum of 0. 
This is always possible if the nim-sum is not zero before the move. If the nim-sum is zero,
then the next player will lose if the other player does not make a mistake. To find out which move to make, 
let X be the nim-sum of all the heap sizes. Find a heap where the nim-sum of X and heap-size is less than the 
heap-size - the winning strategy is to play in such a heap, reducing that heap to the nim-sum of its original size
with X"

'''


# it's a losing state when all numbers xor'ed yield 0. for variale arrays into a list object
n,a,b = eval(input()),list(map(int, input().split())),0

#Only 1 set of moves helps player 1 win. So 1(2,3) -> 2(2,2) -> 3(1,2) -> 4(1,1) -> 5(0,1)
for i in a: b ^= i
print(sum([int(i^b < i) for i in a]))

