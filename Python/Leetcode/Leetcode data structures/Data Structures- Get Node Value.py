#Body
''' 
 Get Node data of the Nth Node from the end.
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the node data of the linked list in the below method.



def GetNode(head, position):
    stk = []
    t = head
    while t:
        stk = [t.data] + stk
        t = t.next
    return stk[position]
  
''' 
  
  
def GetNode(head, position):
    index = 0
    result = head
    while(head.next != None):
        head = head.next
        index += 1
        if(index > position):
            result = result.next
    
    return result.data
  