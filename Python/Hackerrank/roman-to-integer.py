#https://leetcode.com/problems/roman-to-integer/description/
class Solution:
    def romanToInt(self, s):
        """
        :type s: str
        :rtype: int
        """
        translator = {
            "I": 1,
            "V": 5,
            "X": 10,
            "L": 50,
            "C": 100,
            "D": 500,
            "M": 1000
        }
        
        total = 0
        previous = 0
        for c in s:
            value = translator[c]
            total += value
            # for strings like "IV" you have to subtract the "I" that you added previously, twice
            if previous < value:
                total -= 2*previous
            previous = value
        return total 
