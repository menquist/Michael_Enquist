"""
You’re given the pointer to the head nodes of two linked lists. Compare the data in the nodes of the linked lists to check if they are equal. The lists are equal only if they have the same number of nodes and corresponding nodes contain the same data. Either head pointer given may be null meaning that the corresponding list is empty.

Input Format 
You have to complete the int CompareLists(Node* headA, Node* headB) method which takes two arguments - the heads of the two linked lists to compare. You should NOT read any input from stdin/console.

Output Format 
Compare the two linked lists and return 1 if the lists are equal. Otherwise, return 0. Do NOT print anything to stdout/console.

Sample Input

NULL, 1 --> NULL 
1 --> 2 --> NULL, 1 --> 2 --> NULL

Sample Output

0
1
Explanation 
1. We compare an empty list with a list containing 1. They don't match, hence return 0. 
2. We have 2 similar lists. Hence return 1.


 Merge two linked list
 head could be None as well for empty list
 Node is defined as
 
 class Node(object):
 
   def __init__(self, data=None, next_node=None):
       self.data = data
       self.next = next_node

 return back the head of the linked list in the below method.
"""


  
  
def CompareLists(headA, headB):
    curA = headA
    curB = headB
    while curA and curB:
        if curA.data != curB.data:
            return 0
        curA = curA.next
        curB = curB.next
    return 1 if curA == curB else 0 