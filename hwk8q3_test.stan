// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int n;
  int n_trials;
  array[n] int<lower=1,upper=n_trials> trials;
  array[n] int arms;
  int n_arms;
  array[n] int groupSizes;
  array[n] int numbersOfCases;
  array[n] int is_vaccinated;
  // patients in each arm
  int t_v;
  int t_c;
  // vaccine in use 
  array[n] int is_moderna;
}

// The parameters accepted by the model. 
parameters {
  // priors on effectiveness
  real <lower=0,upper=1> mu_e;
  real <lower=0> lambda_e;
  // priors on incidence
  real mu_p;
  real <lower=0> lambda_p;
  // likelihoods
  real ee;
  real vv;
  // binomials?
  int n_c;
  int n_v;
}

// The model to be estimated. 
model {
  // generate prior values
  mu_e ~ uniform(0, 1);
  lambda_e ~ exponential(0.001);
  // using conversion from beta
  mu_p ~ beta(1, 9);
  lambda_p ~ exponential(0.001);
  for (j in 1:n_vaccines){
      // iterate over plate (what to do here?)
      for (i in 1:n_trials[j]){
        ee ~ beta( lambda_e * mu_e,
                   lambda_e * (1 - mu_e)) ;
        pp ~ beta( lambda_e * mu_e,
                   lambda_e * (1 - mu_e)) ;
        // incidence number total
        n_c[i] ~ binomial(tc[i], p);
        // incidence among vaccinated
        n_v[i] ~ binomial(tv[i], p*(1-ee));
      }
    effectiveness[j] = ee;
  }
}

