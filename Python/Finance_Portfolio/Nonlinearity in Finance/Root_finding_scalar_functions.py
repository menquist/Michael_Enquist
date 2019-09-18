#!/usr/bin/env python
# coding: utf-8

# ## Nonlinearity methods in Finance:
# ### Some root-finding functions that can be found in the scipy.optimize modules are bisect, newton, brentq, and ridder. Let's set up the examples that we have discussed using the implementations by SciPy:

# In[2]:


"""
# Reference from Mastering Python for Finance 

Documentation at
http://docs.scipy.org/doc/scipy/reference/optimize.html
"""
import scipy.optimize as optimize

y = lambda x: x**3 + 3.*x**2 - 6.
dy = lambda x: 3.*x**2 + 6.*x

# Call method: bisect(f, a, b[, args, xtol, rtol, maxiter, ...])
print("Bisection method: %s"       % optimize.bisect(y, -5., 5., xtol=0.00001))

# Call method: newton(func, x0[, fprime, args, tol, ...])
print("Newton's method: %s"       % optimize.newton(y, 5., fprime=dy))
# When fprime=None, then the secant method is used.
print("Secant method: %s"       % optimize.newton(y, 5.))

# Call method: brentq(f, a, b[, args, xtol, rtol, maxiter, ...])
print("Brent's method: %s"       % optimize.brentq(y, -5., 5.))


# #### In solving for nonlinear implied volatility models, volatility values cannot be negative. In active markets, finding a root or a zero of the volatility function is almost impossible without modifying the underlying implementation. In such cases, implementing our own root-finding methods might perhaps give us more authority over how our application should behave.
