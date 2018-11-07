#https://leetcode.com/problems/excel-sheet-column-number/description/
#Given a column title as appear in an Excel sheet, return its corresponding column number.
class Solution(object):
    def titleToNumber(self, s):
        """
        :type s: str
        :rtype: int
        """
        result = 0 
        for i in xrange(len(s)):
            result *= 26
            result += ord(s[i]) - ord('A') + 1
        return result    
