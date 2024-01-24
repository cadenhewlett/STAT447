library(distr)




mc_estimate <- function(f){
  M <- 100000
  m_vals <- runif(M)
  G_m <- f(m_vals)
  G_hat_m <- (1/M)*sum(G_m)
  return(G_hat_m)
}

mc_estimate(sin)
cos(0)-cos(1)

f <- function(x){
  1 / ( ((x^2)*(1 - x))^(1/3) )
}
f(0.5)
set.seed(19690720)
expected <- pi / (sin(pi/3))
observed = mc_estimate(f)
(abs(observed - expected) / expected)*100

##########

suppressPackageStartupMessages(library(distr))

## Utilities to make the distr library a bit nicer to use

p <- function(distribution, realization) {
  d(distribution)(realization) # return the PMF or density 
}

Bern = function(probability_to_get_one) {
  DiscreteDistribution(supp = 0:1, prob = c(1-probability_to_get_one, probability_to_get_one))
}

## Key functions called by simPPLe programs

# Use simulate(distribution) for unobserved random variables
simulate <- function(distribution) {
  r(distribution)(1) # sample once from the given distribution
}

# Use observe(realization, distribution) for observed random variables
observe = function(realization, distribution) {
  # `<<-` lets us modify variables that live in the global scope from inside a function
  weight <<- weight * p(distribution, realization) 
}

# create the coin (Bernoulli distribution)
coin <- Bern(1/2)
# flip the coin once
flip <- simulate(coin)
# compute the probability of the flip
prob <- p(coin, flip)
# report the findings
print(paste("The coin flipped a ", flip, 
      ". The probability of this flip is ",
       prob, sep = ""))

coin_flips = rep(0, 4) # "dataset" of four identical coin flips = (0, 0, 0, 0) 

# simPPLe's description of our "bag of coins" example
my_first_probabilistic_program = function() {
  
  # Similar to forward sampling, but use 'observe' when the variable is observed
  coin_index = simulate(DiscreteDistribution(supp = 0:2))
  for (i in seq_along(coin_flips)) { 
    prob_heads = coin_index/2
    observe(coin_flips[i], Bern(1 - prob_heads)) 
  }
  
  # return the test function g(x, y)
  return(ifelse(coin_index == 1, 1, 0))
}

posterior = function(ppl_function, number_of_iterations) {
  numerator = 0.0
  denominator = 0.0
  # add simple indicator function
  g <- function(x){x == 1}
  for (i in 1:number_of_iterations) {
    # Step 1: Simulate Iteration m
    m <- simulate(ppl_function)
    # Step 2: Compute G^m = g(X^m)
    G <- g(m)
    # Step 3: Compute gamma(X^m) p(X)L(y|x)
    gamma <- p(ppl_function, m)
    # Step 4: Compute w(X^m) = gamma(X^m)/q(X^m)
    weight <<- 1.0
    observe(m, ppl_function)
    # Step 5: Add this to our rolling weight total
    denominator <<- denominator + weight
    # Step 6: Update the rolling numerator
    numerator <<- numerator + (w*G)
  }
  return(numerator/denominator)
}
posterior = function(ppl_function, number_of_iterations) {
  numerator = 0.0
  denominator = 0.0
  for (i in 1:number_of_iterations) {
    weight <<- 1.0
    m <- ppl_function()
    observe(m, ppl_function())
  }
  return(numerator/denominator)
}

posterior(my_first_probabilistic_program, 1)


# numerator = 0.0
# denominator = 0.0
# for(i in 1:1){
#   m <- 1
#   g <- function(x){x == 1}
#   gamma <- p(my_first_probabilistic_program, m)
#   weight <<- 1.0
#   observe(m, my_first_probabilistic_program)
#   denominator <<- denominator + weight
#   numerator <<- numerator + (weight*g(m))
# }
# numerator / denominator

