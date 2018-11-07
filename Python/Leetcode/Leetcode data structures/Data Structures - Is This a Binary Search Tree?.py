
'''

For the purposes of this challenge, we define a binary tree to be a binary search tree with the following ordering requirements:

The data  value of every node in a node's left subtree is less than the data value of that node.
The  data  value of every node in a node's right subtree is greater than the data value of that node.
Given the root node of a binary tree, can you determine if it's also a binary search tree?

Complete the function in your editor below, which has  parameter: a pointer to the root of a binary tree. It must return a boolean denoting whether or not the binary tree is a binary search tree. You may have to write one or more helper functions to complete this challenge.


Node is defined as
class node:
  def __init__(self, data):
      self.data = data
      self.left = None
      self.right = None

def check_binary_search_tree_(root):
'''



def check_binary_search_tree_(root):
    return checkBST(root, -1, 10001)

def checkBST(root, Min, Max):
    if not root:
        return True
    if root.data <= Min or root.data >= Max:
        return False
    return checkBST(root.left, Min, root.data) and checkBST(root.right, root.data, Max)