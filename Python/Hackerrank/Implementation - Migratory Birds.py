
'''
You have been asked to help study the population of birds migrating across the continent. Each type of bird you are interested in will be identified by an integer value. Each time a particular kind of bird is spotted, its id number will be added to your array of sightings. You would like to be able to find out which type of bird is most common given a list of sightings.If two or more types of birds are equally common, choose the type with the smallest ID number.

Function Description

Complete the migratoryBirds function in the editor below. It has the following description:

Parameters	Name	Type	Description
n	Integer	Number of elements in the input array.
ar	Integer Array	Respective type numbers of each bird in the flock.
Return	The function must return an integer denoting the type number of the most common bird.
Constraints


It is guaranteed that each type is , , , , or .
Raw Input Format

The first line contains an integer denoting , the number of birds sighted and reported in the array . 
The second line describes  as  space-separated integers representing the type numbers of each bird sighted.

Sample Input 0

6
1 4 4 4 5 3
Sample Output 0

4
Explanation 0

The different types of birds occur in the following frequencies:

Type :  bird
Type :  birds
Type :  bird
Type :  birds
Type :  bird

The type number that occurs at the highest frequency is type , so we print  as our answer.
'''

#!/bin/python3


n = int(input().strip())
types = list(map(int, input().strip().split(' ')))
count = {}
for t in types:
    if t in list(count.keys()):
        count[t] += 1
    else:
        count[t] = 1

# to find the max value in the array
max_value = max(count.values())
required_keys = [k for k, v in count.items() if v == max_value]
print(min(required_keys))


