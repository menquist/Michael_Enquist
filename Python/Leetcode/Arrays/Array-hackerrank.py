##########################################################Array - DS
"""
An array is a type of data structure that stores elements of the same type
in a contiguous block of memory.
In an array, A, of size N, each memory location has some unique index, i (where 0 <= i < N),
that can be referenced as A[i] (you may also see it written as Ai).
Given an array A, of size N, of integers:
print each element in reverse order as a single line of space-separated integers.
"""

N = int(input().strip())
A = input().split(' ')
print(" ".join(A[::-1]))


##########################################################2D Array - DS
"""
Given a 2D Array, A, we define an hourglass in to be a subset of values
with indices falling in this pattern in A's graphical representation:
a b c
  d
e f g
There are hourglasses in A, and an hourglass sum is the sum of an hourglass' values.
"""

A = []
for arr_i in range(6):
	arr_t = [int(arr_temp) for arr_temp in input().strip().split(' ')]
	A.append(arr_t)

smax = -9 * 7

for row in range(len(A) - 2):
	for column in range(len(A[row]) - 2):
		tl = A[row][column]
		tc = A[row][column + 1]
		tr = A[row][column + 2]
		mc = A[row + 1][column + 1]
		bl = A[row + 2][column]
		bc = A[row + 2][column + 1]
		br = A[row + 2][column + 2]
		s = tl + tc + tr + mc + bl + bc + br
		smax = max(s, smax)

print(smax)
##########################################################Dynamic Array
"""
Create a list, seqList, of N empty sequences, where each sequence is indexed from 0 to N-1.
The elements within each of the N sequences also use 0-indexing.
Create an integer, lastAns, and initialize it to 0.
The types of queries that can be performed on your list of sequences (seqList) are described below:
1] Query: 1 . y
	1] Find the sequence, seq, at index ((x xor lastAns) % N) in seqList.
	2] Append integer y to sequence seq
2] Query: 2 . y
	1] Find the sequence, seq, at index ((x xor lastAns) % N) in seqList.
	2] Find the value of element y % size in seq (where size is the size of seq) and assign it to lastAns.
	3] Print the new value of lastAns on a new line
"""

N, Q = map(int, input().split())
lastAns = 0
seqList = [[] for _ in range(N)]

for __ in range(Q):
	q, x, y = map(int, input().split())
	index = (x ^ lastAns) % N
	seq = seqList[index]
	if q == 1:
		seq.append(y)
	elif q == 2:
		size = len(seq)
		lastAns = seq[y % size]
		print(lastAns)
	else:
		raise ValueError()

##########################################################Left Rotation
"""
Given an array of size N and a number d, rotate the array to the left by d
i.e. shift the array elements to the left by d.
Ex: The array [1, 2, 3, 4, 5] after rotating by 2 gives [3, 4, 5, 1, 2].
"""


N, d = map(int, input().split())
a = list(input().split())
r = a[d % N : N] + a[0 : d % N]
print(*r, sep=" ")


##########################################################Sparse Arrays
"""
There are N strings.
Each string's length is no more than 20 characters.
There are also Q queries.
For each query, you are given a string.
You need to find out how many times this string occurred previously.
"""


def count(target, source):
	counter = 0
	for item in source:
		if item == target:
			counter += 1
	return counter


N = int(input())
strings = []
for _ in range(N):
	string = input()
	strings.append(string)

Q = int(input())
for __ in range(Q):
	query = input()
	occurrences = count(query, strings)
	print(occurrences)
##########################################################Multiplication Arrays	
'''You are given a list of size N, initialized with zeroes. You have to perform M operations on the list and output the maximum of final values of all the N elements in the list. For every operation, you are given three integers a, b and k. The value k needs to be added to all the elements ranging from index a through b (both inclusive).

Input Format

The first line will contain two integers N and M separated by a single space.
The next M lines will each contain three integers a, b and k separated spaces.
The numbers in the list are numbered from 1 to N.

Output Format

A single integer on a separate line line containing maximum value in the updated list.

Constraints

3 ≤ N ≤ 10^7
1 ≤ M ≤ 2 * 10^5
1 ≤ a ≤ b ≤ N
0 ≤ k ≤ 10^9
Sample input:

5 3
1 2 100
2 5 100
3 4 100
Sample output:

200
Explanation

After first update list will be 100 100 0 0 0.

After second update list will be 100 200 100 100 100.

After third update list will be 100 200 200 200 100.

So the required answer will be 200.
'''	
#https://www.hackerrank.com/challenges/crush/problem
'''
see, you are adding sum to a[p] and adding negative sum at a[q+1]. 
which make sure that when you add element from a[p] to a[q] sum is added 
only once and it should be subtracted at a[q+1] as this sum span from p to q
only. Rest array element are either 0 or some other input sum. max of addition will be output. 
refer to above code for p, q, and sum.

Instead of storing the actual values in the array, you store the difference
between the current element and the previous element. So you add sum to a[p]
showing that a[p] is greater than its previous element by sum. 
You subtract sum from a[q+1] to show that a[q+1] is less than a[q] by 
sum (since a[q] was the last element that was added to sum). By the end of 
all this, you have an array that shows the difference between every 
successive element. By adding all the positive differences, you get the
value of the maximum element
'''

from itertools import accumulate

n, m = map(int, input().split(' '))
dx = [0] * (n + 1) # allow run past end

for _ in range(m):
    a, b, c = map(int, input().split(' '))
    dx[a - 1] += c
    dx[b] -= c

print(max(accumulate(dx)))  	