######### IMPLEMENTATION TEST ###########
library(extraDistr)
library(distr)
#set.seed(100)

mu = 2; lambda = 0.1; alpha = 0.8; beta = 0.7
library(lestat)
# n, mu, lambda, alpha, beta
rho = c(1, 2, 0.1, 0.8, 0.7)

rGN = function(n, mu_0, lambda, alpha, beta){
  tau = rgamma(n, alpha, beta)
  mu = rnorm(n, mu_0, sd = sqrt(1/(tau*lambda)))
  return( list(mu = mu, tau = tau) )
}
# 
P = do.call(rGN, as.list(rho))

test =rnorm(100000, P$mu, sqrt(1/P$tau))
mean(test)

# we decide upon Q-value sampling and paired with moment updating, as the other pair of selection and update methods
# require implementation difficulty far beyond the constraints of this work. 

# the paper inspects accumulated rewards and the mean and variance therein solely in its experiment results 
# to provide an attempt at an extension on this work, we will evaluate additional policy interests for Q-learning
