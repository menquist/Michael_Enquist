#https://leetcode.com/problems/palindrome-number/description/
class Solution:
    def isPalindrome(self, x):
        """
        :type x: int
        :rtype: bool
        """
        if x < 0: return False
        return x == int(str(x)[::-1])
    
        