# https://leetcode.com/problems/reverse-string/description/
class Solution:
    def reverseString(self, s):
        """
        :type s: str
        :rtype: str
        """
        lst = list(s)
        result = []
        i = len(lst)
        while i > 0:
            i -= 1 
            result.append(lst[i])
        return "".join(result)  