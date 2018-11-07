"""

You’re given the pointer to the head node of a linked list. Change the next pointers of the nodes so that their order is reversed. The head pointer given may be null meaning that the initial list is empty.

Input Format 
You have to complete the Node* Reverse(Node* head) method which takes one argument - the head of the linked list. You should NOT read any input from stdin/console.

Output Format 
Change the next pointers of the nodes that their order is reversed and return the head of the reversed linked list. Do NOT print anything to stdout/console.

Sample Input

NULL 
2 --> 3 --> NULL

Sample Output

NULL
3 --> 2 --> NULL

 Reverse a linked list
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the head of the linked list in the below method.
"""

def Reverse(head):
    if not head or not head.next: return head
    newHead = Reverse(head.next)
    head.next.next = head
    head.next = None
    return newHead
  