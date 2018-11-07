'''
Each time Sunny and Johnny take a trip to the Ice Cream Parlor, they pool together  dollars for ice cream. 
On any given day, the parlor offers a line of  flavors. Each flavor, , is numbered sequentially with a unique ID 
number from  to  and has a cost, , associated with it.

Given the value of  and the cost of each flavor for  trips to the Ice Cream Parlor, help Sunny and Johnny 
choose two flavors such that they spend their entire pool of money () during each visit. For each trip to the parlor,
print the ID numbers for the two types of ice cream that Sunny and Johnny purchase as two space-separated integers 
on a new line. You must print the smaller ID first and the larger ID second.

Note: Two ice creams having unique IDs  and  may have the same cost (i.e., ).
The first time, they pool together  dollars. There are five flavors available that day and flavors  
and have a total cost of .

Thus, we print 1 4 on a new line.
The second time, they pool together  dollars. There are four flavors available that day and flavors 
and have a total cost of . Thus, we print 1 2 on a new line.
'''

from itertools import combinations

test_cases = int(input())

for test_case in range(test_cases):
    dollars = int(input())
    total_flavors = int(input())  # unused variable
    flavors_costs = input()
    # string transformed into an array of ints
    flavors_costs = [int(x) for x in flavors_costs.split(' ')]

    # generation of combinations of 2 flavors
    flavors_combinations = combinations(flavors_costs, 2)

    for flavors_tuple in flavors_combinations:
        # two distinct flavors whose cost equals `dollars`
        #if ((flavors_tuple[0] + flavors_tuple[1]) == dollars):
        if ((flavors_tuple[0] + flavors_tuple[1]) == dollars):
            # get the index in flavors_costs for the first flavor
            first_flavor = flavors_costs.index(flavors_tuple[0])
            # get the index in flavors_costs for the second flavor.
            # since both flavors might cost the same, second_flavor must be
            # searched discarding the position of first_flavor
            second_flavor = flavors_costs.index(flavors_tuple[1],
                                                first_flavor + 1)
            # indices are indexed from 1
            print(("{} {}".format(first_flavor + 1, second_flavor + 1)))
            # unique solution
            break