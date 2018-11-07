'''
Objective 
In the previous challenge, we calculated a mean. In this challenge, we practice calculating a weighted mean. Check out the Tutorial tab for learning materials and an instructional video!

Task 
Given an array, , of  integers and an array, , representing the respective weights of 's elements, calculate and print the weighted mean of 's elements. Your answer should be rounded to a scale of  decimal place (i.e.,  format).

Input Format

The first line contains an integer, , denoting the number of elements in arrays  and . 
The second line contains  space-separated integers describing the respective elements of array . 
The third line contains  space-separated integers describing the respective elements of array .

Constraints


, where  is the  element of array .
, where  is the  element of array .
Output Format

Print the weighted mean on a new line. Your answer should be rounded to a scale of  decimal place (i.e., format).
'''


n = int(input())
x = list(map(int, input().split(' ')))
w = list(map(int, input().split(' ')))

#calculating a weighted mean. 
'''Return a zip object whose .__next__() method returns a tuple where
the i-th element comes from the i-th iterable argument.  The .__next__()
method continues until the shortest iterable in the argument sequence
is exhausted and then it raises StopIteration.'''

print(round(sum([number * weight for number, weight in zip(x,w)])/ sum(w),1)) 