
############ QUESTION 1 #############
library(extraDistr)
set.seed(19690720)
forward_sample <- function(){
      X = rdunif(1, 0, 2)
      return(
        c(X, sapply(1:4, function(x) rbinom(n = 1, size = 1, prob = X/2)))
      )
}

# simulate
simulations = sapply(1:20000, function(x){
                  sample_i = forward_sample()
                  g_x = (1 + sample_i[2])^sample_i[1]
                  return(g_x)
                })
simulated_mean = mean(simulations)
print(paste("Simulated Value:", simulated_mean))
print(paste("Calculated Value:", round(13/6, 4)))
print(paste("Percent Difference: ", 
            round(abs(simulated_mean- 13/6)/(13/6)*100, 4), "%", sep = ''))

############ QUESTION 4 #############

posterior_given_four_heads <- function(rho){
  kvals = 0:(length(rho)-1)
  K = length(kvals)
  # kvals = ( 0:(K-1) )
  biases = kvals / (K-1)
  # in this case, the denominator is from LOTP again
  p_E =  sum( biases^4 * rho )
  # then numerator is P(X)P(E | X)
  prior_likeli =  sapply(1:K, function(k){ biases[k]^4 * rho[k]} ) 
  # then return the ratio
  return(prior_likeli / p_E)
}

# this returns q1
posterior_given_four_heads(rho = c(1/3, 1/3, 1/3))
# 1/17
# this is q3? no.
# posterior_given_four_heads(
#   rep(1/100, times = 100)
# )

# this is q3
# posterior_given_four_heads(
#   rho = c(1/100, 98/100, 1/100)
# )
# rho is 1/100, 98/100, 1/100

# but then it thinks there are three
# q4, part 4
posterior_given_four_heads((1:10))
# we might say prop to because it is prop to some continuous distribution, i.e.

  