
library(extraDistr)
library(ggplot2)
library(pracma)
library(distr)
library(latex2exp)
source("scaffold.R")
source("simple.R")
source("simple_utils.R")
test = Norm(0, 1)
launches = c(1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1)

beta_1 = simulate(Norm(0, 1))
beta_0 = simulate(Norm(0, 1))
sapply(1:length(launches),
       function(L){
         observe(launches[L],
                 Bern(plogis(beta_0 + beta_1*L)))
       })

logistic_regression = function() {
  beta_1 = simulate(Norm(0, 1))
  beta_0 = simulate(Norm(0, 1))
  sapply(1:length(launches),
         function(L){
           observe(launches[L],
                   Bern(plogis(beta_0 + beta_1*L)))
         })
  
  return(c(beta_0, beta_1, weight))
}

posterior(logistic_regression, 100)
