p <- function(distribution, realization) {
  d(distribution)(realization) # return the PMF or density 
}

Bern = function(probability_to_get_one) {
  DiscreteDistribution(supp = 0:1, prob = c(1-probability_to_get_one, probability_to_get_one))
}
# Use simulate(distribution) for unobserved random variables
simulate <- function(distribution) {
  r(distribution)(1) # sample once from the given distribution
}

# Use observe(realization, distribution) for observed random variables
observe = function(realization, distribution) {
  # `<<-` lets us modify variables that live in the global scope from inside a function
  weight <<- weight * p(distribution, realization) 
}
############# 
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


weight <- 1
posterior = function(ppl_function, number_of_iterations) {
  numerator = 0.0
  denominator = 0.0
  g = function(x){x == 1}
  for (i in 1:number_of_iterations) {
    weight <<- 1.0
    m <- ppl_function()
    G <- g(m)
    # update numerator and denominator
    numerator <- numerator + (weight*G)
    denominator <- denominator + weight
  }
  return(numerator/denominator)
}
posterior(my_first_probabilistic_program, 100)
