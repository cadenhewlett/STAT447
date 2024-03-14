
logistic_regression <- function(X){
  # get the matrix sizes
  n_pred = ncol(X)
  n_obs = nrow(X)
  
  # simulate beta values
  beta = rnorm(n_pred)
  
  # compute the p parameter of the bernoulli distribution
  p = plogis(X%*%beta) # 1 / (1 + exp(-X%*%beta))
  
  # then the y values
  y = rbinom(n_obs, size = 1, prob = p)
  
  # then take the mean
  ybar = mean(y)
  
  # and return it
  return(ybar)
}


set.seed(1928)
# declare parameters
M = 100000
p = 0.9
m = 100
n_pred = c(1, 2, 4, 15)
K = length(n_pred)
# rename the logistic_regression to match the terminology provided 
f <- logistic_regression
remove(logistic_regression)

# create the set of matrices described earlier: (1)
X <- sapply(n_pred, function(n){
  matrix(data = rbinom(n*m, size = 1, prob = p),
         nrow = m, ncol = n)
})

# then, create Y: k in [1, K], m in [1, M] : (2)
Y = matrix(rep(1:K, each = M), nrow = M, ncol = K, byrow = F)
# finally, populate this matrix: (3)
Ybar = apply(Y,  MARGIN = c(1,2), function(k){ f(X[[k]]) })
head(Ybar)

par(mfrow=c(1, K))

for (k in 1:K) {
  hist(Ybar[, k],  breaks=20, ylim=c(0,17000), 
       main=paste("Histogram for n =", n_pred[k]), xlab="Average Outcome", ylab="Frequency")
}


## no - if we want to infer on new data, i.e. use posterior as prior, 
## cromwell's rule is sad now. 
## it also tells me our fits are tending towards homogeneity. 
## the average is getting closer and closer to zero or one respectively.
## opinioned prior
## this means the sums X^T B are getting closer and closer to +/- infinity
## it seems to me like this is an example of over-fitting
## this could be fixed by doing Cross-Validation (LOO-CV is used in machine learning
## and I imagine there are similar techniques for Bayesian LOO model fititng)
## doing this for each model in the step
## or spike-and-slab as we did previously, for selecting which predictors are
## actually relevant.

## animorph into a sideways crocodile
c(plogis(-Inf), plogis(Inf))
## Bayesian version of lasso maybe?
