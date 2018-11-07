"""
 Merge two linked lists
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the head of the linked list in the below method.
 
 Input (stdin)
3
4
1 3 5 6
3
2 4 7
1
15
1
12
0
2
1 2
Your Output (stdout)
1 2 3 4 5 6 7 
12 15 
1 2 
Expected Output
1 2 3 4 5 6 7
12 15
1 2
 
"""

def MergeLists(headA, headB):
    if not headA or not headB:
        head = headA or headB
        return head
    head = min([headA, headB], key = lambda x:x.data)
    head.next = MergeLists(head.next, max([headA, headB], key = lambda x:x.data))
    return head
  
  
  
  
  
  
  
  