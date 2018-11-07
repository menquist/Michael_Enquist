"""
 Print elements of a linked list on console
 head input could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node
 
 
Input (stdin)
5
0
2
1 2
4
2 1 4 5
3
34 45 56
5
1 2 3 4 5
Your Output (stdout)
1
2
2
1
4
5
34
45
56
1
2
3
4
5
Expected Output
1
2
2
1
4
5
34
45
56
1
2
3
4
5 
 
"""
def print_list(head):
    if head:
        print(head.data)
        print_list(head.next)