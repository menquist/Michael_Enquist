#https://leetcode.com/problems/single-number/description/
#Given an array of integers, every element appears twice except for one. Find that single one.
class Solution(object):
    def singleNumber(self, nums):
        return reduce(lambda x, y: x ^ y, nums)