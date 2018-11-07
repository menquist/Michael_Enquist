'''
Consider an array of numeric strings, , where each string is a positive 
number with anywhere from  to  digits. Sort the array's elements in
non-decreasing (i.e., ascending) order of their real-world integer values 
and print each element of the sorted array on a new line.
'''


'''
Convert a number or string x to an integer, or return 0 if no arguments are given. 
If x is a number, return x.__int__(). 
For floating point numbers, this truncates towards zero.
'''
n =  int(input().strip())
unsorted = []
lengths = []


'''
Return a str version of object. See str() for details.
str is the built-in string class.
For general information about strings, see Text Sequence Type — str.

'''
for k in range(n):
    number = str(input().strip())
    unsorted.append(number)
    lengths.append(len(number))
    
'''
The left-to-right evaluation order of the iterables is guaranteed.
This makes possible an idiom for clustering a data series into n-length 
groups using zip(*[iter(s)]*n).
'''
arr = zip(lengths,unsorted)

for i in sorted(arr):
    print(i[1])

    