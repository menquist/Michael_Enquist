#https://leetcode.com/problems/reverse-integer/description/

class Solution:
    def reverse(self, x):
        """
        :type x: int
        :rtype: int
        """
        
        sign = (x >0) - (x <0)
        abs_val = sign * x
        
        ans = 0
        while(abs_val !=0):
            temp = ans*10 + abs_val%10
            if(temp >= 2**31):
                return 0
            ans = temp
            abs_val //= 10
            
        return sign*ans

if __name__ == "__main__":
    print Solution().reverse(123)
    print Solution().reverse(-321)    
