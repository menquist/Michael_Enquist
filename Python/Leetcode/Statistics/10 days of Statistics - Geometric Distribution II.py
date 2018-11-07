'''
first 5 default
'''

import math
e = list(map(float, input().split()))
f = int(input())

def prob_during_first(prob_def, n):
	prob_not_def = 1 - prob_def
	return math.pow(prob_not_def, n-1) * prob_def

val = [prob_during_first(e[0]/e[1], i) for i in range(1, 5+1)]
print(round(sum(val),3))