library(extraDistr)
K = 20;
prior_probabilities = sapply(0:K, function(k){(k / K)*(1 - k/K)})
plot(prior_probabilities)
# n choose x, p^x, (1-p)^{n - x}