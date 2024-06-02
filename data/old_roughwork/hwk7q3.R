# prior: Beta(alpha, beta)
alpha = 1
beta = 2 

# observations: binomial draws
n_successes = 3 
n_trials = 3
#set.seed(447)
gamma_beta_binomial = function(p) {
  if (p < 0 || p > 1) return(0.0)
  dbeta(p, alpha, beta) * dbinom(x = n_successes, size = n_trials, prob = p)
}




Q <- function(x){  rnorm(1, mean = x) }
R <- function(x_tilde, x_m, gamma_fn){ gamma_fn(x_tilde) / gamma_fn(x_m) }
A <- function(R){ rbinom(1, size = 1, p = min(1, R)) }
# simple Metropolis-Hastings algorithm (normal proposal)
simple_mh = function(gamma, initial_point, n_iters) {
  X = numeric(n_iters) 
  dim = length(initial_point)
  
  X[1] = initial_point
  
  for (m in 2:(n_iters+1)){
    # declare proposed x value
    x_m <- X[m-1]
    # compute q(X^{m-1})
    x_tilde <- Q(x_m)
    # compute ratio \gamma(\tilde{X}^{m}) / \gamma(X^{m-1})
    R_m <- R(x_tilde, x_m, gamma_fn = gamma)
    # compute bernoulli trial at p = min\{1, R^{(m)} \}
    A_m <- A(R_m)
    # accept or reject proposal 
    X[m] <- ifelse(A_m == 1,  x_tilde, x_m)
  }
  return(X)
}

# source("../exercises/ex07_scaffold.R")
# source("../../solutions/sol07_mh.R")

samples = simple_mh(gamma_beta_binomial, 0.5, 1000)

plot(samples[1:1000], 
     type = "o", 
     col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2))
print( mean(samples) )
?rgb
