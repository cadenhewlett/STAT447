//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> n;         // number of trials
  int<lower=0,upper=n> k; // number of successes
}

parameters {
  real<lower=0,upper=1> p;
}

model {
  // prior
  p ~ beta(1,1);

  // likelihood
  k ~ binomial(n, p);
}
