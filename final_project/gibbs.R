# Non-parametric Poisson regression using Dirichlet Process mixture model
alpha = 10  # DP concentration parameter
H = 1/100   # rate  parameter for base measure

# Function to generate from base distribution G_0
G_0 <- function(n) rexp(n, rate = H)

# Function to generate stick-breaking weights
generate_p <- function(n, alpha) {
  b <- rbeta(n, 1, alpha)
  p <- numeric(n)
  p[1] <- b[1]
  p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
  return(p)
}


n <- 100
p <- generate_p(n, alpha)
theta <- G_0(n)
weights <- cbind(theta, p)  

X <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 20) 


M <- 1000

cluster_assignments <- sample(1:n, size = length(X), replace = TRUE)

samples_theta <- matrix(NA, nrow = M, ncol = n)

for (iter in 1:M) {
  # Update cluster assignments
  for (i in 1:length(X)) {
    # Calculate the likelihood for each cluster
    likelihoods <- dpois(X[i], lambda = theta)
    # Calculate the full conditional probabilities
    probs <- likelihoods * p
    probs <- probs / sum(probs)
    cluster_assignments[i] <- sample(1:n, size = 1, prob = probs)
  }
  
  # Update theta parameters for each cluster
  for (k in 1:n) {
    cluster_data <- X[cluster_assignments == k]
    if (length(cluster_data) > 0) {
      # Posterior parameters for gamma distribution
      shape <- sum(cluster_data) + 1  # +1 from prior (Exponential -> Gamma)
      rate <- length(cluster_data) + H
      theta[k] <- rgamma(1, shape, rate)
    } else {
      # No data points assigned to this cluster, sample from prior
      theta[k] <- G_0(1)
    }
  }
  
  # Update weights p using the stick-breaking process
  b <- rbeta(n, 1 + vapply(1:n, function(k) sum(cluster_assignments == k), numeric(1)), alpha + vapply(1:n, function(k) sum(cluster_assignments > k), numeric(1)))
  p[1] <- b[1]
  p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
  
  # Store sampled parameters
  samples_theta[iter, ] <- theta
}

# Analysis and diagnostics
print(mean(samples_theta, na.rm = TRUE))

plot(samples_theta)

cluster_means <- apply(samples_theta, 2, mean, na.rm = TRUE)
print(cluster_means)

