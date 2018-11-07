
'''
Steve wants to reduce s as much as possible. 
To do this, he will repeat the above operation as many times as it can 
be performed. Help Steve out by finding and printing s‘s non-reducible form!

In this problem, to get the fully reduced string, we have to use the stack. 
Suppose we are at the ith character of the string.Suppose we are at the 
ith position of the string. If the character at top of the stack is same 
as S[i], we pop the top element of the stack and move to i+1th position. 
Otherwise, if the stack is an empty or top element of the stack is not 
equal to S[i], we will push S[i] to the top of the stack and then move to 
i+1th position.
'''
        
import sys

def super_reduced_string(s):
    res = [] # stack
    for c in s:
        if res and res[len(res)-1] == c: # peek what's on top
            res.pop()
        else:
            res.append(c)    
    res = ''.join(res)
    return res or 'Empty String'
    

s = input().strip()
result = super_reduced_string(s)
print(result)