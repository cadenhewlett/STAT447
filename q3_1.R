library(distr)

# Apollo 11 moon landing, as an integer


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

