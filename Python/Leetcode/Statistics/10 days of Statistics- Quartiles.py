'''
Objective 
In this challenge, we practice calculating quartiles. Check out the Tutorial tab for learning materials and an instructional video!

Task 
Given an array, , of  integers, calculate the respective first quartile (), second quartile (), and third quartile (). It is guaranteed that , , and  are integers.

Input Format

The first line contains an integer, , denoting the number of elements in the array. 
The second line contains  space-separated integers describing the array's elements.

Constraints


, where  is the  element of the array.
Output Format

Print  lines of output in the following order:

The first line should be the value of .
The second line should be the value of .
The third line should be the value of .

'''
def median(a, l):
    if not l % 2:
        return int((a[l//2-1] + a[l//2]) / 2)
    else:
        return int(a[l//2])

n = int(input())
array = list(sorted(map(int, input().split(" "))))
if not n % 2:
    left,middle,right = array[:n//2],None,array[n//2]
else:
    left,middle,right = array[:n//2],array[n//2], array[(n//2) + 1:]
    
    
print(median(left, len(left)))
print(middle or median(array, n))
print(median(right, len(right)))    


#####################################################################################################

def median(a, l):
    if not l % 2:
        return float((a[l//2-1] + a[l//2]) / 2)
    else:
        return float(a[l//2])

n = int(input())
x = list(map(int, input().split(" ")))
f = list(map(int, input().split(" ")))
s = []
for xi, fi in zip(x, f):
    s.extend([xi] * fi)

s.sort()
n = len(s)

if not n % 2:
    left, _, right = s[:n//2], None, s[n//2:]
else:
    left, _, right = s[:n//2], s[n//2], s[(n//2)+1:]

print(abs(median(right, len(right)) - median(left, len(left))))
