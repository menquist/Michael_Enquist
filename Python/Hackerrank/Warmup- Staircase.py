'''
Consider a staircase of size :

   #
  ##
 ###
####
Observe that its base and height are both equal to , and the image is drawn using # symbols and spaces. The last line is not preceded by any spaces.

Write a program that prints a staircase of size .

Input Format

A single integer, , denoting the size of the staircase.

Output Format

Print a staircase of size  using # symbols and spaces.

Note: The last line must have  spaces in it.
'''

class Solution(object):
    #create step for starcase function for each row

    def staircase(self, n):
        result = ''
        for i in range(1, n+1):
            for j in range(1, n+1):
                result += '#' if n-i < j else ' '
            result += '\n'
        return result
    # staircase function into array output for n     

    def main(self):
        n = int(input())
        result = self.staircase(n)
        print(result, end='')

if __name__ == '__main__':
    Solution().main()