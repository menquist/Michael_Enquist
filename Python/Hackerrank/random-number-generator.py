# https://www.hackerrank.com/challenges/random-number-generator/problem
# Enter your code here. Read input from STDIN. Print output to STDOUT
import sys

def gcd(x, y):
    if y == 0:
        return x
    else:
        return gcd(y, x%y)

def prob(a,b,c):
    if a+b <= c:
        return 1,1
    if a >= c and b >= c:
        x = c * c
        y = 2 * a * b
        d = gcd(x,y)
        return x/d, y/d
    
    if a <= c and b <= c:
        x = 2* a*b -  (a - c + b)**2
        y  = 2 *a *b 
        d = gcd(x,y)
        return x/d, y/d
    
    a, b = max(a,b), min(a,b)
    x = c**2 - (c-b)**2
    y = 2 * a* b
    d = gcd(x,y)
    return x/d, y/d

n = int( raw_input().strip() )

for i in range(n):
    a,b,c, = map(int, raw_input().strip().split())
    
    x,y = prob(a,b,c)
    print "{}/{}".format(x,y)




import sys


def gcd(a, b):
    return b and gcd(b, a % b) or a

if __name__ == '__main__':
    T = int(sys.stdin.readline())
    
    for _ in range(T):
        A, B, C = [int(x) for x in sys.stdin.readline().split()]
        
        # Ensure that A <= B
        A, B = min(A, B), max(A, B)
        domain = 2 * A * B
        
        # Compute the area of the rectangle [0, A] x [0, B] within y <= C - x
        if C >= A + B:
            print('1/1')
        
        else:
            if C <= A:
                cover = C * C
            elif A < C <= B:
                cover = (2 * A * (C - A)) + A**2
            else:
                cover = (2 * A * (C - A)) + (2 * (A + B - C) * (C - B)) + (A + B - C)**2
            
            n = gcd(cover, domain)
            print('%d/%d' % (cover // n, domain // n))
