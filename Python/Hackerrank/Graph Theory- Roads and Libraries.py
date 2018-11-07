'''
The Ruler of HackerLand believes that every citizen of the country should have access to a library. Unfortunately, HackerLand was hit by a tornado that destroyed all of its libraries and obstructed its roads! As you are the greatest programmer of HackerLand, the ruler wants your help to repair the roads and build some new libraries efficiently.
HackerLand has  cities numbered from  to . The cities are connected by  bidirectional roads. A citizen has access to a library if:
Their city contains a library.
They can travel by road from their city to a city containing a library.
The following figure is a sample map of HackerLand where the dotted lines denote obstructed roads:
image
The cost of repairing any road is  dollars, and the cost to build a library in any city is  dollars.
You are given  queries, where each query consists of a map of HackerLand and value of  and .
For each query, find the minimum cost of making libraries accessible to all the citizens and print it on a new line.
Input Format
The first line contains a single integer, , denoting the number of queries. The subsequent lines describe each query in the following format:
The first line contains four space-separated integers describing the respective values of  (the number of cities),  (the number of roads),  (the cost to build a library), and  (the cost to repair a road).
Each line  of the  subsequent lines contains two space-separated integers,  and , describing a bidirectional road connecting cities  and .


After reading the problem and understanding DFS algorithm from the discussion above, 
the solution will be much easier to understand. Here the damaged roads are edges and the cities are nodes. 
A graph may contain several clusters.  A cluster is a set of nodes that are somehow connected to each other 
(directly or indirectly) but not to other nodes in different clusters.

So each cluster can be considered as a sub-problem / sub-task. 
Solution for all the clusters will be the final solution. 

A cluster will have at least one library. So the solution will be at least cost_of_library times the number of 
clusters. After this initial consideration, we are now to decide whether to build a library in each city or to 
repair the roads of a cluster. If there are 'c' citites in a cluster, there will be c-1 roads.
Now if cost_of_library < cost_of_road, we are better off building c-1 additional libraries in the other
c-1 cities adding (c-1)*cost_of_library to our total cost  and if cost_of_library > cost_of_road, we just repair 
the c-1 roads adding (c-1)*cost_of_road to our total cost.

Now we are to determine the clusters. Here DFS comes into play. We start DFS on non-visited nodes and mark the visited ones. When we start new DFS on a non-visited node, we are traversing a new cluster because previous nodes of other clusters , determined by previous DFS, are unreachable from this new node. After determining the number of nodes in a cluster (using DFSUtil function) , we at first add a library's cost and then depending on the condition either add road reparation cost or add additional library cost.

After traversing all the nodes, we have finally determined the optimal cost.
'''


#!/bin/python3

import sys




def DFS(graph, start):
    S = set([])
    Q = []
    Q.append(start)
    while Q:
        current = Q.pop(0)
        if current not in S:
            S.add(current)
            if len(graph[current]) != 0:
                for n in graph[current]:
                    Q.insert(0, n)
    return list(S)

#import of the data
#Loop over each graph
q = int(input().strip())
for a0 in range(q):
    n, m, x, y = input().strip().split(' ')
    n, m, x, y = [int(n), int(m), int(x), int(y)]
    graph_list = {}
    #Loop over the edges of each graph
    for a1 in range(m):
        city_1, city_2 = input().strip().split(' ')
        city_1, city_2 = [int(city_1) - 1, int(city_2) - 1]
        #creating a dict of connected nodes for each key node
        if city_1 in graph_list:
            graph_list[city_1] += [city_2]
        else:
            graph_list[city_1] = [city_2]
        if city_2 in graph_list:
            graph_list[city_2] += [city_1]
        else:
            graph_list[city_2] = [city_1]

    subgraphs_list = []
    #loop to determine the number of node of each connected subgraph
    while graph_list:
        subgraph = 0
        b = DFS(graph_list, list(graph_list.keys())[0])
        for n in b:
            graph_list.pop(n)
            subgraph += 1
        subgraphs_list.append(subgraph)
    cost = 0
    for sub in subgraphs_list:
        # print(len(sub))
        cost += (min(y * (sub - 1) + x, x * (sub)))
    print(cost)  