#!/usr/bin/env python
# coding: utf-8

# ### The Newton-Raphson method:
# #### The Newton-Raphson method, uses an iterative procedure to solve for a root using information about the derivative of a function. The derivative is treated as a linear problem to be solved. 

# In[1]:



""" The Newton-Raphson method """
""" # Reference from Mastering Python for Finance """

def newton(f, df, x, tol=0.001, maxiter=100):
    """
    :param f: The function to solve
    :param df: The derivative function of f
    :param x: Initial guess value of x
    :param tol: The precision of the solution
    :param maxiter: Maximum number of iterations
    :return: The x-axis value of the root,
                number of iterations used
    """
    n = 1
    while n <= maxiter:
        x1 = x - f(x)/df(x)
        if abs(x1 - x) < tol:  # Root is very close
            return x1, n
        else:
            x = x1
            n += 1

    return None, n

if __name__ == "__main__":
    y = lambda x: x**3 + 3*x**2 - 5
    dy = lambda x: 3*x**2 + 6*x
    root, iterations = newton(y, dy, 5.0, 0.00001, 100)
    print("Root is:", root)
    print("Iterations:", iterations)


# #### With Newton's method, we obtained a really close solution with less iteration over the bisection method and the incredmental method.
