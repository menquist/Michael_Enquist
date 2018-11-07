'''

Colleen is having a birthday! She will have a cake with one candle for each year of her age. When she blows out the candles, she’ll only be able to blow out the tallest ones.

Find and print the number of candles she can successfully blow out.

Input Format

integer

Colleen's age 

 space-separated integers

candle heights 

Output Format

Print the number of candles Colleen blows out.



import sys

def birthdayCakeCandles(n, ar):
    # Complete this function

n = int(input().strip())
ar = list(map(int, input().strip().split(' ')))
result = birthdayCakeCandles(n, ar)
print(result)
'''


#!/bin/python3

import sys
from collections import Counter


# persons's age in n and ar in space-separated integers
def birthdayCakeCandles(n, ar):
    d = Counter(ar)
    largest_key = max(Counter.keys(d))
    return d.get(largest_key)
    # Complete this function

n = int(input().strip())
ar = list(map(int, input().strip().split(' ')))
result = birthdayCakeCandles(n, ar)
print(result)



