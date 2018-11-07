'''

Sorting 
One common task for computers is to sort data. For example, people might want to see all their files 
on a computer sorted by size. Since sorting is a simple problem with many different possible solutions,
it is often used to introduce the study of algorithms.

Insertion Sort 
These challenges will cover Insertion Sort, a simple and intuitive sorting algorithm.
We will first start with an already sorted list.

Insert element into sorted list 
Given a sorted list with an unsorted number  in the rightmost cell, can you write some simple code to insert  
into the array so that it remains sorted?

Print the array every time a value is shifted in the array until the array is fully sorted. The goal of
this challenge is to follow the correct order of insertion sort.

Guideline: You can copy the value of  to a variable and consider its cell "empty". 
Since this leaves an extra cell empty on the right, you can shift everything over until  can be inserted. 
This will create a duplicate of each value, but when you reach the right spot, you can replace it with .

Input Format 
There will be two lines of input:

 - the size of the array
 - the array containing  sorted integers and  unsorted integer  in the rightmost cell
Output Format 
On each line, output the entire array every time an item is shifted in it.

Constraints 
 


#!/bin/python3

import sys

def insertionSort1(n, arr):
    # Complete this function

if __name__ == "__main__":
    n = int(input().strip())
    arr = list(map(int, input().strip().split(' ')))
    insertionSort1(n, arr)

'''

#Complete the method insertionSort which takes in one parameter:

# Arr- an array with the value  in the right-most cell.
def insertionSort(ar):
    num = ar[-1]
    for i in range(m-2, -1, -1):
        if ar[i] > num:
            ar[i+1] = ar[i]
            print(" ".join(str(j) for j in ar))
        else:
            ar[i+1] = num
            print(" ".join(str(j) for j in ar))
            return
    ar[0] = num
    print(" ".join(str(j) for j in ar))
    return

'''
Evaluate the given source in the context of globals and locals.

The source may be a string representing a Python expression
or a code object as returned by compile().
The globals must be a dictionary and locals can be any mapping,
defaulting to the current globals and locals.
If only globals is given, locals defaults to it.
'''
m = eval(input())

'''
In python you use a split(delimiter) method onto a string in order to get a list based in the delimiter that you specified (by default is the space character) and the strip() method removes the white spaces at the end and beginning of a string

So step by step the operations are:

raw_input()          #' insert 0 5     '
raw_input().strip()  #'insert 0 5'
raw_input().strip().split()  #['insert', '0', '5']
you can use split(';') by example if you want to convert strings delimited by semicolons 'insert;0;5'
'''
ar = [int(i) for i in input().strip().split()]
insertionSort(ar)




#############################################################################################################


'''
In Insertion Sort Part 1, you sorted one element into an array. Using the same approach repeatedly, can you sort an entire unsorted array?

Guideline: You already can place an element into a sorted array. How can you use that code to build up a sorted array, one element at a time? Note that in the first step, when you consider an array with just the first element - that is already "sorted" since there's nothing to its left that is smaller.

In this challenge, don't print every time you move an element. Instead, print the array after each iteration of the insertion-sort, i.e., whenever the next element is placed at its correct position.

Since the array composed of just the first element is already "sorted", begin printing from the second element and on.

Input Format 
There will be two lines of input:

 - the size of the array
 - a list of numbers that makes up the array
Output Format 
On each line, output the entire array at every iteration.

Constraints 
 


Sample Input

6
1 4 3 5 6 2
Sample Output

1 4 3 5 6 2 
1 3 4 5 6 2 
1 3 4 5 6 2 
1 3 4 5 6 2 
1 2 3 4 5 6 
Explanation 
Insertion Sort checks  first and doesn't need to move it, so it just prints out the array. Next,  is inserted next to , and the array is printed out. This continues one element at a time until the entire array is sorted.

Task 
The method insertionSort takes in one parameter: , an unsorted array. Use an Insertion Sort Algorithm to sort the entire array.


#!/bin/python3

import sys

def insertionSort2(n, arr):
    # Complete this function

if __name__ == "__main__":
    n = int(input().strip())
    arr = list(map(int, input().strip().split(' ')))
    insertionSort2(n, arr)


'''
def insertionSort(ar):
    for i in range(1, len(ar)):
        temp = ar[i]
        # Instead, print the array after each iteration of the insertion-sort, i.e., 
        #whenever the next element is #placed at its correct position.Since the array 
        #composed of just the first element is already "sorted", begin printing from the second element and on.
        #Insertion Sort  checks 4 first and doesn't need to move it, so it just prints out the array. 
        #Next, 3 is inserted next to 1 , and the array is printed out. This continues one element at a time until 
        #the entire array is sorted.
        j = i
        while j > 0 and temp < ar[j-1]:
            ar[j] = ar[j-1]
            j -= 1
        ar[j] = temp
        print(' '.join(str(j) for j in ar))

m = eval(input())
ar = [int(i) for i in input().strip().split()]
insertionSort(ar)


