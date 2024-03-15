
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  // maintaining the same formatting as in the tutorial
  int n;
  int n_trial;
  array[n] int<lower=1,upper=n_trial> trial;
  array[n] int arms;
  int n_arms;
  array[n] int groupSizes;
  array[n] int numbersOfCases;
  array[n] int is_vaccinated;
}

parameters {
    // priors on effectiveness
  real mu_e;
  real <lower=0> lambda_e;
  // priors on incidence
  real mu_p;
  real <lower=0> lambda_p;
  // effectiveness and prevalence
  vector<lower=0,upper=1>[n_trial] efficiencies;
  vector<lower=0,upper=1>[n_trial] prevalences;
}

model {
   // generate prior values
  mu_e ~ uniform(0, 1);
  lambda_e ~ exponential(0.001);
  // using conversion from beta
  mu_p ~ beta(1, 9);
  lambda_p ~ exponential(0.001);
  // now, trial aren't just beta (1,1)
  for (t in 1:n_trial) {
    efficiencies[t] ~ beta( lambda_e * mu_e,
                                lambda_e * (1 - mu_e)) ;
    prevalences[t] ~  beta( lambda_p * mu_p,
                                lambda_p * (1 - mu_p)) ;
  }
  // full model, I think
  for (i in 1:n) {
    numbersOfCases[i] ~ binomial(groupSizes[i], (prevalences[trial[i]] *
    (is_vaccinated[i] == 1 ? 1.0 - efficiencies[trial[i]] : 1.0)));
  }
}

// indicator of if Moderna is better than Pfizer, as done previously
generated quantities {
  // pfizer = 3, moderna = 2
  real is_moderna_better = efficiencies[2] > efficiencies[3] ? 1 : 0;
}
