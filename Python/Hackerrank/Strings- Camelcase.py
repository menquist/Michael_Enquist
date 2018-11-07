'''
Alice wrote a sequence of words in CamelCase as a string of letters, , having the following properties:

It is a concatenation of one or more words consisting of English letters.
All letters in the first word are lowercase.
For each of the subsequent words, the first letter is uppercase and rest of the letters are lowercase.
Given , print the number of words in  on a new line.

Input Format

A single line containing string .

Constraints


Output Format

Print the number of words in string .

#!/bin/python

import sys

def camelcase(s):
    print(sum(map(str.isupper, input())) + 1)
'''

import sys


s = input().strip()
res = 0
#A single line containing string s. Return the number of items in a container.l
if len(s) > 0:
    res = 1
    
# All letters in the first word are lowercase.  # 1 =< s =< 10^5    
for i in range(1, len(s)):
    #For each of the subsequent words, the first letter is uppercase and rest of the letters are lowercase.
    #Return the Unicode code point for a one-character string.
    if ord(s[i]) >= ord('A') and ord(s[i]) <= ord('Z'):
        res += 1
print(res)    