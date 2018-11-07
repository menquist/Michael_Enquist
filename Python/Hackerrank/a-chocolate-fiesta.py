# https://www.hackerrank.com/challenges/a-chocolate-fiesta/problem
from sys import stdin
 
N = stdin.readline()
Array = stdin.readline().split()
OddCount = 0
 
for a in Array:
    if int(a) % 2 == 1:
        OddCount += 1
 
OddSets = 0 if OddCount == 0 else (2**(OddCount - 1)) - 1
EvenSets = (2**(int(N) - OddCount)) - 1
 
NumberOfEvenSumSets = EvenSets if OddSets == 0 else (EvenSets * OddSets) \
                                                    + EvenSets + OddSets
 


print (NumberOfEvenSumSets % ((10**9) + 7))
