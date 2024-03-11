library(distr)
# https://tokic.com/www/tokicm/publikationen/papers/AdaptiveEpsilonGreedyExploration.pdf
# watkins - learning from delayed rewards
#https://inria.hal.science/hal-00840479/document
## Simple Example of Bayesian Reinforcement ##
set.seed(14)
par(mfrow = c(1, 2))
x = x_old = 1
y = y_old = 2
simple_course = matrix(
  c(1, 1, 1, 0), nrow = 1
)
simple_course[x, y] = 2

simple_order = matrix(
  c(0, 0, 1, 2), nrow = 1
)

## Truth Rewards - Unkown to Agent
R = apply(simple_order, MARGIN = c(1,2), 
          FUN = function(cell){
            if (cell == 0){ runif(1, min = -2, max = -1)  } 
            else{ 2*cell + runif(1, min = -0.5, max = 0.5) }
          })
R
# n Unif(0, 1) distributions
Ri_s = sapply(1:3,
              function(cell){ distr::Unif(0, 1) })

# Simple Action Set
A = list(
  RIGHT <- function(){y <<- y + 1;},
  LEFT <- function(){y <<- y - 1; },
  STAY <- function(){}
)
VERBOSE = c("RIGHT", "LEFT", "STAY")

Omega = matrix(0, nrow = length(unlist(simple_course)),
               ncol = length(A))
# Populate Each Cell of Omega with a unif(0, 1) distribution
Omega = apply(Omega, MARGIN = c(1,2),
              function(cell){
                cell = distr::Unif(0, 1) 
              })
# assume a prior on temperature
tau <- distr::Exp(0.001)
obsv_temp = distr::r(tau)(1)

# of Omega, the row indicates the state

expec_rewards = sapply(Omega[1, ], function(R_i){  r(R_i)(1) })

# boltzmann as a function of expected rewards at time t
btzm = exp( expec_rewards/obsv_temp )

choice = distr::DiscreteDistribution(
        supp = 1:length(A),
        prob = btzm/sum(btzm)
      )

hist( r(choice)(1000) )
# NOTE : A[r(choice)(1)][[1]]() : subsets and runs the function all at once
a_t = r(choice)(1)
y_old = y
print(paste("Pre-Action Distribution: unif(",
             distr::Min( Omega[y, a_t][[1]] ), ", ",
             distr::Max( Omega[y, a_t][[1]] ), ").", sep = ""))
print(paste("Taking Action:", print(VERBOSE[a_t])))
A[a_t][[1]]()
print(paste("Observed Reward:", round( R[x, y] ,3)))

if( R[x, y] < distr::Min( Omega[y_old, a_t][[1]] ) ){
  print("Lower Reward than Prior Min! Updating Min...")
  a_stored <-  distr::Min( Omega[y_old, a_t][[1]] )
  distr::Min( Omega[y_old, a_t][[1]] ) <- R[x, y]
  distr::Max( Omega[y_old, a_t][[1]] ) <- a_stored
} else if( R[x, y] > distr::Max( Omega[y_old, a_t][[1]] ) ){
  print("Higher Reward than Prior Max! Updating Max...")
  b_stored <- distr::Max( Omega[y_old, a_t][[1]] )
  distr::Max( Omega[y_old, a_t][[1]] ) <- R[x, y]
  distr::Min( Omega[y_old, a_t][[1]] ) <- b_stored
}

print(paste("Pre-Action Distribution: unif(",
            round(distr::Min( Omega[y_old, a_t][[1]] ), 2), ", ",
            round(distr::Max( Omega[y_old, a_t][[1]] ), 2), ").", sep = ""))
## Then, we would repeat the process! ##
## This bit here is assuming that we are at the "old" y again
example_iteration = sapply(Omega[2, ], function(R_i){  r(R_i)(1) })
example_iteration
## Notice that the simulated rewards are different now ^

## letting temp = 1 for ease of operations
btzm_example = exp( example_iteration )


## now the probabilities have been re-weighted
btzm_example/sum(btzm_example)

##

choice_example = distr::DiscreteDistribution(
  supp = 1:length(A),
  prob = btzm_example/sum(btzm_example)
)
btzm_example/sum(btzm_example)
# much less likely to choose to Stay
post_vals = r(choice_example)(5000)
hist( post_vals )
