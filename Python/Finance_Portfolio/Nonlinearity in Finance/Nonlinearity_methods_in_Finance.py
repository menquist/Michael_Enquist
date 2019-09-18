#!/usr/bin/env python
# coding: utf-8

# ## Nonlinearity in Finance
# #### Some nonlinear models commonly used for studying economics and financial time series. From the model data given in continuous time, the intention is therefore to search for the extrema that could possibly infer valuable information. The incremental search root-finder method is a basic demonstration of the fundamental behavior of a root-finding algorithm.
# 
# ### Incremental search algorithm:

# In[3]:


""" An incremental search algorithm """
""" # Reference from Mastering Python for Finance """
import numpy as np


def incremental_search(f, a, b, dx):
    """
    :param f: The function to solve
    :param a: The left boundary x-axis value
    :param b: The right boundary x-axis value
    :param dx: The incremental value in searching
    :return: The x-axis value of the root,
                number of iterations used
    """
    fa = f(a)    
    c = a + dx 
    fc = f(c)    
    n = 1

    while np.sign(fa) == np.sign(fc):
        if a >= b:
            return a - dx, n
        
        a = c
        fa = fc
        c = a + dx
        fc = f(c)
        n += 1

    if fa == 0:
        return a, n
    elif fc == 0:
        return c, n
    else:
        return (a + c)/2., n

if __name__ == "__main__":
    """
    The keyword 'lambda' creates an anonymous function
    with input argument x
    """
    y = lambda x: x**3 + 3.0*x**2 - 6.
    root, iterations = incremental_search(y, -5., 5., 0.001)
    print("Root is:", root)
    print("Iterations:", iterations)


# #### The incremental search root-finder method is a basic demonstration of the fundamental behavior of a root-finding algorithm.

# ### The bisection method:
# #### The bisection method is considered the simplest one-dimensional root-finding algorithm.

# In[4]:


""" The bisection method """
""" # Reference from Mastering Python for Finance """

def bisection(f, a, b, tol=0.1, maxiter=10):
    """
    :param f: The function to solve
    :param a: The x-axis value where f(a)<0
    :param b: The x-axis value where f(b)>0
    :param tol: The precision of the solution
    :param maxiter: Maximum number of iterations
    :return: The x-axis value of the root,
                number of iterations used
    """
    c = (a+b)*0.5  # Declare c as the midpoint ab
    n = 1  # Start with 1 iteration
    while n <= maxiter:
        c = (a+b)*0.5
        if f(c) == 0 or abs(a-b)*0.5 < tol:
            # Root is found or is very close
            return c, n

        n += 1
        if f(c) < 0:
            a = c
        else:
            b = c
                
    return c, n

if __name__ == "__main__":
    y = lambda x: x**3 + 3*x**2 - 6
    root, iterations = bisection(y, -5, 5, 0.00001, 100)
    print("Root is:", root)
    print("Iterations:", iterations)


# #### As we can see, the result from the bisection method gives us better precision in far fewer iterations over the incremental search method. The biggest advantage of using the bisection method is its guarantee to converge to an approximation of the root, given a predetermined error tolerance level and the maximum number of iterations allowed.
# 
# ### The secant root-finding method:
# 
# #### The secant method uses secant lines to find the root. A secant line is a straight line that intersects two points of a curve. This method can be thought of as a Quasi-Newton method.

# In[6]:


""" The secant root-finding method """
""" # Reference from Mastering Python for Finance """

def secant(f, a, b, tol=0.001, maxiter=100):
    """
    :param f: The function to solve
    :param a: Initial x-axis guess value
    :param b: Initial x-axis guess value, where b>a
    :param tol: The precision of the solution
    :param maxiter: Maximum number of iterations
    :return: The x-axis value of the root,
                number of iterations used
    """
    n = 1
    while n <= maxiter:
        c = b - f(b)*((b-a)/(f(b)-f(a)))
        if abs(c-b) < tol:
            return c, n

        a = b
        b = c
        n += 1

    return None, n

if __name__ == "__main__":
    y = lambda x: x**3 + 3*x**2 - 6
    root, iterations = secant(y, -5.0, 5.0, 0.00001, 100)
    print("Root is:", root)
    print("Iterations:", iterations)


# #### Though all of the preceding root-finding methods gave very close solutions, the secant method performs with fewer iterations compared to the bisection method, but with more than Newton's method. 
