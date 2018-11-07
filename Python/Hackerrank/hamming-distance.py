#https://leetcode.com/problems/hamming-distance/description/
class Solution:
    def hammingDistance(self, X, Y):
        """
        :type X: int
        :type Y: int
        :rtype: int
        """
        distance = 0
        Z = X ^ Y
        while Z:
            distance += 1
            Z &= Z - 1
        return distance    