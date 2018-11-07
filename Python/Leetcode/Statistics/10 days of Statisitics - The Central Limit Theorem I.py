
'''
Objective 
In this challenge, we practice solving problems based on the Central Limit Theorem. 
Check out the Tutorial tab for learning materials!

Task 
A large elevator can transport a maximum of 9800  pounds. 
Suppose a load of cargo containing 49 boxes must be transported via the 
elevator. The box weight of this type of cargo follows a distribution with a
mean of 205  pounds and a standard deviation of 15  pounds. Based on this information, what is the probability 49 
that all boxes can be safely loaded into the freight elevator and transported?
'''

import math

def less_than_boundary_cdf(x, mean, std):
    return round(0.5 * (1 + math.erf((x - mean)/ (std * math.sqrt(2)))), 4)

print(less_than_boundary_cdf(9800, 49 * 205, math.sqrt(49) * 15))