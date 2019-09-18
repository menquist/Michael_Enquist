#!/usr/bin/env python
# coding: utf-8

# ## Different types of StockOption classes:
# ####  • Pricing European and American options using a binomial tree
# #### • Using a Cox-Ross-Rubinstein (CRR) binomial tree
# #### • Pricing options using a Leisen-Reimer (LR) tree
# #### • Pricing options using a trinomial tree
# #### • Pricing options using a binomial and trinomial lattice
# #### • Deriving Greeks from a tree for free
# 
# 
# #### The current underlying price, strike price, risk-free rate, time to maturity, and number of time steps are compulsory common attributes for pricing options.
# 
# #### Example: A non-dividend paying stock price starts at 50, and in each of the two time steps, the stock may go up by 20 percent or go down by 20 percent. We suppose that the risk-free rate is 5 percent per annum and the time to maturity T is 0.5 years. We would like to find the value of an European put option with a strike price K of 52.

# In[ ]:


"""
README
======
 # Reference from Mastering Python for Finance 
======
"""

""" Store common attributes of a stock option """
import math


class StockOption(object):

    def __init__(self, S0, K, r, T, N, params):
        self.S0 = S0
        self.K = K
        self.r = r
        self.T = T
        self.N = max(1, N) # Ensure N have at least 1 time step
        self.STs = None  # Declare the stock prices tree

        """ Optional parameterss used by derived classes """
        self.pu = params.get("pu", 0)  # Probability of up state
        self.pd = params.get("pd", 0)  # Probability of down state
        self.div = params.get("div", 0)  # Divident yield
        self.sigma = params.get("sigma", 0)  # Volatility
        self.is_call = params.get("is_call", True)  # Call or put
        self.is_european = params.get("is_eu", True)  # Eu or Am

        """ Computed values """
        self.dt = T/float(N)  # Single time step, in years
        self.df = math.exp(
            -(r-self.div) * self.dt)  # Discount factor


# ### Binomial European Option class:
# #### The Python implementation of the binomial option pricing model of an European option is given as the BinomialEuropeanOption class, which inherits the common attributes of the option from the StockOption class.

# In[ ]:




""" Price a European option by the binomial tree model """
from StockOption import StockOption
import math
import numpy as np


class BinomialEuropeanOption(StockOption):

    def _setup_parameters_(self):
        """ Required calculations for the model """
        self.M = self.N + 1  # Number of terminal nodes of tree
        self.u = 1 + self.pu  # Expected value in the up state
        self.d = 1 - self.pd  # Expected value in the down state
        self.qu = (math.exp((self.r-self.div)*self.dt) -
                   self.d) / (self.u-self.d)
        self.qd = 1-self.qu

    def _initialize_stock_price_tree_(self):
        # Initialize terminal price nodes to zeros
        self.STs = np.zeros(self.M)

        # Calculate expected stock prices for each node
        for i in range(self.M):
            self.STs[i] = self.S0*(self.u**(self.N-i))*(self.d**i)

    def _initialize_payoffs_tree_(self):
        # Get payoffs when the option expires at terminal nodes
        payoffs = np.maximum(
            0, (self.STs-self.K) if self.is_call
            else (self.K-self.STs))

        return payoffs

    def _traverse_tree_(self, payoffs):
        # Starting from the time the option expires, traverse
        # backwards and calculate discounted payoffs at each node
        for i in range(self.N):
            payoffs = (payoffs[:-1] * self.qu +
                       payoffs[1:] * self.qd) * self.df

        return payoffs

    def __begin_tree_traversal__(self):
        payoffs = self._initialize_payoffs_tree_()
        return self._traverse_tree_(payoffs)

    def price(self):
        """ The pricing implementation """
        self._setup_parameters_()
        self._initialize_stock_price_tree_()
        payoffs = self.__begin_tree_traversal__()

        return payoffs[0] # Option value converges to first node

if __name__ == "__main__":
    from BinomialEuropeanOption import BinomialEuropeanOption
    eu_option = BinomialEuropeanOption(
        50, 50, 0.05, 0.5, 2,
        {"pu": 0.2, "pd": 0.2, "is_call": False})
    print(eu_option.price())


# #### Using the binomial option pricing model gives us a present value of 4.826 for the European put option.
# 
# ### Binomial American Option:
# #### Unlike European options that can only be exercised at maturity, American options can be exercised at any time during their lifetime.
# 

# In[ ]:




""" Price a European or American option by the binomial tree """
from StockOption import StockOption
import math
import numpy as np


class BinomialTreeOption(StockOption):

    def _setup_parameters_(self):
        self.u = 1 + self.pu  # Expected value in the up state
        self.d = 1 - self.pd  # Expected value in the down state
        self.qu = (math.exp((self.r-self.div)*self.dt) -
                   self.d)/(self.u-self.d)
        self.qd = 1-self.qu

    def _initialize_stock_price_tree_(self):
        # Initialize a 2D tree at T=0
        self.STs = [np.array([self.S0])]

        # Simulate the possible stock prices path
        for i in range(self.N):
            prev_branches = self.STs[-1]
            st = np.concatenate((prev_branches*self.u,
                                 [prev_branches[-1]*self.d]))
            self.STs.append(st)  # Add nodes at each time step

    def _initialize_payoffs_tree_(self):
        # The payoffs when option expires
        return np.maximum(
            0, (self.STs[self.N]-self.K) if self.is_call
            else (self.K-self.STs[self.N]))

    def __check_early_exercise__(self, payoffs, node):
        early_ex_payoff =             (self.STs[node] - self.K) if self.is_call             else (self.K - self.STs[node])

        return np.maximum(payoffs, early_ex_payoff)

    def _traverse_tree_(self, payoffs):
        for i in reversed(list(range(self.N))):
            # The payoffs from NOT exercising the option
            payoffs = (payoffs[:-1] * self.qu +
                       payoffs[1:] * self.qd) * self.df

            # Payoffs from exercising, for American options
            if not self.is_european:
                payoffs = self.__check_early_exercise__(payoffs,
                                                        i)

        return payoffs

    def __begin_tree_traversal__(self):
        payoffs = self._initialize_payoffs_tree_()
        return self._traverse_tree_(payoffs)

    def price(self):
        self._setup_parameters_()
        self._initialize_stock_price_tree_()
        payoffs = self.__begin_tree_traversal__()

        return payoffs[0]

if __name__ == "__main__":
    from BinomialTreeOption import BinomialTreeOption
    am_option = BinomialTreeOption(
        50, 50, 0.05, 0.5, 2,
        {"pu": 0.2, "pd": 0.2, "is_call": False, "is_eu": False})
    print(am_option.price())


# #### The American put option is priced at 5.113. Since American options can be exercised at any time and European options can only be exercised at maturity, this added flexibility of American options increases their value over European options in certain circumstances.
# 
# ###  The Cox-Ross-Rubinstein model:
# #### The Cox-Ross-Rubinstein (CRR) model proposes that, over a short period of time in the risk-neutral world, the binomial model matches the mean and variance of the underlying stock. The volatility of the underlying stock, or the standard deviation of returns of the stock.
# 
# #### Consider again the two-step binomial tree. The non-dividend paying stock has a current price of 50 and a volatility of 30 percent. Suppose that the risk-free rate is 5 percent per annum and the time to maturity T is 0.5 years. 

# In[7]:


""" Price an option by the binomial CRR model """
from BinomialTreeOption import BinomialTreeOption
import math


class BinomialCRROption(BinomialTreeOption):

    def _setup_parameters_(self):
        self.u = math.exp(self.sigma * math.sqrt(self.dt))
        self.d = 1./self.u
        self.qu = (math.exp((self.r-self.div)*self.dt) -
                   self.d)/(self.u-self.d)
        self.qd = 1-self.qu

if __name__ == "__main__":
    from BinomialCRROption import BinomialCRROption
    eu_option = BinomialCRROption(
        50, 50, 0.05, 0.5, 2,
        {"sigma": 0.3, "is_call": False})
    print("European put: %s" % eu_option.price())

    am_option = BinomialCRROption(
        50, 50, 0.05, 0.5, 2,
        {"sigma": 0.3, "is_call": False, "is_eu": False})
    print("American put: %s" % am_option.price())


# ### Leisen-Reimer tree:
# #### Dr. Dietmar Leisen and Matthias Reimer proposed a binomial tree model with the purpose of approximating to the Black-Scholes solution as the number of steps increases. It is known as the Leisen-Reimer (LR) tree, and the nodes do not recombine at every alternate step. It uses an inversion formula to achieve better accuracy during tree transversal. A detailed explanation of the formulas is given in the paper Binomial Models For Option Valuation - Examining And Improving Convergence, March 1995, which is available at http://papers.ssrn.com/sol3/papers.cfm?abstract_id=5976.

# In[8]:





""" Price an option by the Leisen-Reimer tree """
from BinomialTreeOption import BinomialTreeOption
import math


class BinomialLROption(BinomialTreeOption):

   def _setup_parameters_(self):
       odd_N = self.N if (self.N%2 == 1) else (self.N+1)
       d1 = (math.log(self.S0/self.K) +
             ((self.r-self.div) +
              (self.sigma**2)/2.) *
             self.T) / (self.sigma * math.sqrt(self.T))
       d2 = (math.log(self.S0/self.K) +
             ((self.r-self.div) -
              (self.sigma**2)/2.) *
             self.T) / (self.sigma * math.sqrt(self.T))
       pp_2_inversion =            lambda z, n:            .5 + math.copysign(1, z) *            math.sqrt(.25 - .25 * math.exp(
               -((z/(n+1./3.+.1/(n+1)))**2.)*(n+1./6.)))
       pbar = pp_2_inversion(d1, odd_N)

       self.p = pp_2_inversion(d2, odd_N)
       self.u = 1/self.df * pbar/self.p
       self.d = (1/self.df - self.p*self.u)/(1-self.p)
       self.qu = self.p
       self.qd = 1-self.p

if __name__ == "__main__":
   from BinomialLROption import BinomialLROption
   eu_option = BinomialLROption(
       50, 50, 0.05, 0.5, 3,
       {"sigma": 0.3, "is_call": False})
   print("European put: %s" % eu_option.price())

   am_option = BinomialLROption(
       50, 50, 0.05, 0.5, 3,
       {"sigma": 0.3, "is_call": False, "is_eu": False})
   print("American put: %s" % am_option.price())


# ## Greeks
# 
# ##### In the binomial tree pricing models that we have covered so far, we traversed up and down the tree at each point in time to determine the node values. From the information at each node, we can reuse these computed values easily. One such use is the computation of Greeks. Two particularly useful Greeks for options are delta and gamma. Delta measures the sensitivity of the option price with respect to the underlying asset price. Gamma measures the rate of change in delta with respect to the underlying price.

# In[13]:


import math
import numpy as np


import math

""" 
Stores common attributes of a stock option 
"""
class StockOption(object):
    def __init__(
        self, S0, K, r=0.05, T=1, N=2, pu=0, pd=0, 
        div=0, sigma=0, is_put=False, is_am=False):
        """
        Initialize the stock option base class.
        Defaults to European call unless specified.

        :param S0: initial stock price
        :param K: strike price
        :param r: risk-free interest rate
        :param T: time to maturity
        :param N: number of time steps
        :param pu: probability at up state
        :param pd: probability at down state
        :param div: Dividend yield
        :param is_put: True for a put option,
                False for a call option
        :param is_am: True for an American option,
                False for a European option
        """
        self.S0 = S0
        self.K = K
        self.r = r
        self.T = T
        self.N = max(1, N)
        self.STs = [] # Declare the stock prices tree

        """ Optional parameters used by derived classes """
        self.pu, self.pd = pu, pd
        self.div = div
        self.sigma = sigma
        self.is_call = not is_put
        self.is_european = not is_am

    @property
    def dt(self):
        """ Single time step, in years """
        return self.T/float(self.N)

    @property
    def df(self):
        """ The discount factor """
        return math.exp(-(self.r-self.div)*self.dt) 
    

""" 
Price a European or American option by the binomial tree 
"""
class BinomialTreeOption(StockOption):

    def setup_parameters(self):
        self.u = 1+self.pu  # Expected value in the up state
        self.d = 1-self.pd  # Expected value in the down state
        self.qu = (math.exp(
            (self.r-self.div)*self.dt)-self.d)/(self.u-self.d)
        self.qd = 1-self.qu

    def init_stock_price_tree(self):
        # Initialize a 2D tree at T=0
        self.STs = [np.array([self.S0])]

        # Simulate the possible stock prices path
        for i in range(self.N):
            prev_branches = self.STs[-1]
            st = np.concatenate(
                (prev_branches*self.u, 
                 [prev_branches[-1]*self.d]))
            self.STs.append(st) # Add nodes at each time step

    def init_payoffs_tree(self):
        if self.is_call:
            return np.maximum(0, self.STs[self.N]-self.K)
        else:
            return np.maximum(0, self.K-self.STs[self.N])

    def check_early_exercise(self, payoffs, node):
        if self.is_call:
            return np.maximum(payoffs, self.STs[node] - self.K)
        else:
            return np.maximum(payoffs, self.K - self.STs[node])

    def traverse_tree(self, payoffs):
        for i in reversed(range(self.N)):
            # The payoffs from NOT exercising the option
            payoffs = (payoffs[:-1]*self.qu + 
                       payoffs[1:]*self.qd)*self.df

            # Payoffs from exercising, for American options
            if not self.is_european:
                payoffs = self.check_early_exercise(payoffs,i)

        return payoffs

    def begin_tree_traversal(self):
        payoffs = self.init_payoffs_tree()
        return self.traverse_tree(payoffs)

    def price(self):
        """  The pricing implementation """
        self.setup_parameters()
        self.init_stock_price_tree()
        payoffs = self.begin_tree_traversal()
        return payoffs[0]
    
import math

""" 
Price an option by the Leisen-Reimer tree
"""
class BinomialLROption(BinomialTreeOption):

    def setup_parameters(self):
        odd_N = self.N if (self.N%2 == 0) else (self.N+1)
        d1 = (math.log(self.S0/self.K) +
              ((self.r-self.div) +
               (self.sigma**2)/2.)*self.T)/\
            (self.sigma*math.sqrt(self.T))
        d2 = (math.log(self.S0/self.K) +
              ((self.r-self.div) -
               (self.sigma**2)/2.)*self.T)/\
            (self.sigma * math.sqrt(self.T))

        pbar = self.pp_2_inversion(d1, odd_N)
        self.p = self.pp_2_inversion(d2, odd_N)
        self.u = 1/self.df * pbar/self.p
        self.d = (1/self.df-self.p*self.u)/(1-self.p)
        self.qu = self.p
        self.qd = 1-self.p

    def pp_2_inversion(self, z, n):
        return .5 + math.copysign(1, z)*            math.sqrt(.25 - .25*
                math.exp(
                    -((z/(n+1./3.+.1/(n+1)))**2.)*(n+1./6.)
                )
            )


# In[17]:


import numpy as np

""" 
Compute option price, delta and gamma by the LR tree 
"""
class BinomialLRWithGreeks(BinomialLROption):

    def new_stock_price_tree(self):
        """
        Creates an additional layer of nodes to our
        original stock price tree
        """
        self.STs = [np.array([self.S0*self.u/self.d,
                              self.S0,
                              self.S0*self.d/self.u])]

        for i in range(self.N):
            prev_branches = self.STs[-1]
            st = np.concatenate((prev_branches*self.u,
                                 [prev_branches[-1]*self.d]))
            self.STs.append(st)

    def price(self):
        self.setup_parameters()
        self.new_stock_price_tree()
        payoffs = self.begin_tree_traversal()

        # Option value is now in the middle node at t=0
        option_value = payoffs[len(payoffs)//2]

        payoff_up = payoffs[0]
        payoff_down = payoffs[-1]
        S_up = self.STs[0][0]
        S_down = self.STs[0][-1]
        dS_up = S_up - self.S0
        dS_down = self.S0 - S_down

        # Calculate delta value
        dS = S_up - S_down
        dV = payoff_up - payoff_down
        delta = dV/dS

        # calculate gamma value
        gamma = ((payoff_up-option_value)/dS_up - 
                 (option_value-payoff_down)/dS_down) / \
            ((self.S0+S_up)/2. - (self.S0+S_down)/2.)

        return option_value, delta, gamma
    
eu_call = BinomialLRWithGreeks(50, 52, r=0.05, T=2, N=300, sigma=0.3)
results = eu_call.price()

print('European call values')
print('Price: %s\nDelta: %s\nGamma: %s' % results)


# ## Trinomial trees in options pricing
# ###  A class for the trinomial tree options pricing model

# In[21]:


import math
import numpy as np

class TrinomialTreeOption(BinomialTreeOption):

    def setup_parameters(self):
        """ Required calculations for the model """
        self.u = math.exp(self.sigma*math.sqrt(2.*self.dt))
        self.d = 1/self.u
        self.m = 1
        self.qu = ((math.exp((self.r-self.div) *
                             self.dt/2.) -
                    math.exp(-self.sigma *
                             math.sqrt(self.dt/2.))) /
                   (math.exp(self.sigma *
                             math.sqrt(self.dt/2.)) -
                    math.exp(-self.sigma *
                             math.sqrt(self.dt/2.))))**2
        self.qd = ((math.exp(self.sigma *
                             math.sqrt(self.dt/2.)) -
                    math.exp((self.r-self.div) *
                             self.dt/2.)) /
                   (math.exp(self.sigma *
                             math.sqrt(self.dt/2.)) -
                    math.exp(-self.sigma *
                             math.sqrt(self.dt/2.))))**2.

        self.qm = 1 - self.qu - self.qd

    def init_stock_price_tree(self):
        # Initialize a 2D tree at t=0
        self.STs = [np.array([self.S0])]

        for i in range(self.N):
            prev_nodes = self.STs[-1]
            self.ST = np.concatenate(
                (prev_nodes*self.u, [prev_nodes[-1]*self.m,
                                     prev_nodes[-1]*self.d]))
            self.STs.append(self.ST)

    def traverse_tree(self, payoffs):
        # Traverse the tree backwards 
        for i in reversed(range(self.N)):
            payoffs = (payoffs[:-2] * self.qu +
                       payoffs[1:-1] * self.qm +
                       payoffs[2:] * self.qd) * self.df

            if not self.is_european:
                payoffs = self.check_early_exercise(payoffs,i)

        return payoffs
    
eu_put = TrinomialTreeOption(
    50, 52, r=0.05, T=2, N=2, sigma=0.3, is_put=True)
print('European put:', eu_put.price())

am_option = TrinomialTreeOption(50, 52, 
    r=0.05, T=2, N=2, sigma=0.3, is_put=True, is_am=True)
print('American put:', am_option.price())


# In[ ]:




