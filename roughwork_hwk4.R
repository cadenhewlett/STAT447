launches = c(1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1)
library(distr)
library(latex2exp)
#source("scaffold.R")
source("simple.R")
source("simple_utils.R")

logistic_regression = function() {
  beta_1 = simulate(distr::Norm(0, 1))
  beta_0 = simulate(distr::Norm(0, 1))
  sapply(1:length(launches),
         function(L) {
           observe(launches[L],
                   Bern(plogis(beta_0 + beta_1 * L)))
         })
  next_p = plogis(beta_0 + beta_1 * 12)
  return(c(beta_0, beta_1, simulate(Bern(next_p))))
}
logistic_regression()
# posterior(logistic_regression, 1000)

posterior(logistic_regression, 100)


launches = c(launches, 1)
# next_launch_post = posterior(logistic_regression, 2000)

rbind(c("beta_0", "beta_1", "success"), next_launch_post)
plogis(next_launch_post[1] + next_launch_post[2]*12)
posterior(logistic_regression, 1000)

simplified = function() {
  beta_0 = simulate(distr::Norm(0, 1))
  sapply(1:length(launches),
         function(L) {
           observe(launches[L],
                   Bern(plogis(beta_0)))
         })
  next_p = plogis(beta_0)
  return(c(beta_0, simulate(Bern(next_p))))
}
simplified()

##  FOR THE LAST PART ##
##  SET BOTH AT 1/2  ##
##  BUT MAKE THIS A VARIABLE ##
##  WITH DECLARATIONS AT 1/2 ##
##  AND LET IT EVOLVE OVER TIME ##
##  THEN PARAMETER OF PREFER_UNIFIED IS BERN(p) ##
##  WHERE p IS 1/2 FOR UNIFIED AT START ##
##  BASICALLY RUN A BERNOULLI TRIAL BERN(p) ##
##  IF 1, USE UNIFIED, IF 0, USE OTHER ONE
##  SAVE THIS AS AN OUTPUT, BUT ALSO p

## wait even better just multiply
## make p_result a scaling 
## preference prob
## times slope by simulated result
## run it
prefer_p <- 1/2
idea = function() {
  choose_log = simulate(Bern(1/2))
  beta_1 = simulate(distr::Norm(0, 1))
  beta_0 = simulate(distr::Norm(0, 1))

  sapply(1:length(launches),
         function(L) {
           observe(launches[L],
                   Bern(plogis(beta_0 + choose_log * beta_1 * L)))
         })
  next_p = plogis(beta_0 + beta_1 * 12)
  return(c(beta_0, beta_1, choose_log))
}
idea()

## because basically we set up Bern(p) where p = probability prefer log
## our prior is that preference ~ Bern(1/2), i.e. no preference
## however, we run some simulations....

## less than 0.50 -> prefers no log
## 0.50 -> no preference, posterior prob = prior
## more than 0.50 -> prefers log
set.seed(447)
## so freaking cool
posterior(idea, 1000)

