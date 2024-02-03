# suppressPackageStartupMessages(library(distr))

#
# Use observe(realization, distribution) for observed random variables


# Utilities to make the distr library a bit nicer to use
weight <- 1
p <- function(distribution, realization) {
  d(distribution)(realization) # return the PMF or density 
}

observe = function(realization, distribution) {
  # `<<-` lets us modify variables that live in the global scope from inside a function
  weight <<- weight * p(distribution, realization) 
}

Bern = function(probability_to_get_one) {
  DiscreteDistribution(supp = 0:1, prob = c(1-probability_to_get_one, probability_to_get_one))
}

## Key functions called by simPPLe programs

# Use simulate(distribution) for unobserved random variables
simulate <- function(distribution) {
  r(distribution)(1) # sample once from the given distribution
}

########## my simPPLe code


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


# set.seed(1)
# data = rep(0, 4)
# beta_binomial <- function(){
#   p = simulate(Beta(1, 1))
#   for (i in seq_along(data)){
#     observe(data[i], Bern(p))
#   }
#   return(p)
# }
# # key version
# posterior = function(ppl_function, number_of_iterations) {
#   numerator = 0.0
#   denominator = 0.0
#   for (i in 1:number_of_iterations) {
#     weight <<- 1.0
#     G <- ppl_function()
#     # update numerator and denominator
#     numerator <- numerator + (weight*G)
#     denominator <- denominator + weight
#   }
#   return(numerator/denominator)
# }
# #posterior(beta_binomial, 100)


