
results = sapply(1:200, function(iter){
  source("project_outline.R")
})

print( mean( unlist( results[1, ] ) ) )

# Changing epsilon, well-tuned parameters otherwise
# M = 1000, alpha = 0.75, gamma = 0.25. 
# no epsilon: 0.00, mean = 1.66
# low-no epsilon: 0.01, mean = 2.63
# low epsilon: 0.02, mean = 3.11
# low-ish epsilon: 0.05, mean = 4.31
# mid-low epsilon: 0.10, mean = 4.56
# mid epsilon : 0.15, mean = 4.70
# mid epsilon : 0.20, mean = 4.81
# mid epsilon : 0.25, mean = 4.80 
# mid epsilon : 0.30, mean = 4.84
# mid epsilon : 0.35, mean = 4.92
# mid epsilon : 0.40, mean = 4.93
# high epsilon : 0.80, mean = 5
# fully random : 1.00, mean = 5
epsilon_vals = c(0.00, 0.01, 0.02, 0.05, 0.10, 0.15, 
                 0.20, 0.25, 0.30, 0.35, 0.40, 0.80, 1.00)
performance = c(1.66, 2.63, 3.11, 4.31, 4.56, 4.70, 4.81, 4.80, 4.84,
                4.92, 4.93, 5, 5)
# roughly logarithmic
# plot(epsilon_vals, performance)
## Why: even in fully random action, agent still learns rewards pattern 
## because it is well tuned, so, when assessed on optimal policy
## (full exploitation) it will know what to do since M is very large 
## with respect to the overall learning environment. 

# Changing alpha, well-tuned parameters otherwise
# M = 1000,  gamma = 0.25, epsilon = 0.20
# no alpha: 0.00 : 0.98
# low-no alpha: 0.01: 2.54
# low alpha: 0.02 : 2.59
# low-ish alpha: 0.05 : 2.66
# mid-low alpha: 0.10 : 3.34
# mid alpha : 0.15, mean = 3.81
# mid alpha : 0.20, mean = 4.115
# mid alpha : 0.25, mean = 4.405 
# mid alpha : 0.30, mean = 4.51
# mid alpha : 0.35, mean = 4.62
# mid alpha : 0.40, mean = 4.56
# mid alpha : 0.45, mean = 4.675
# mid alpha : 0.50, mean = 4.655
# mid alpha : 0.60, mean = 4.73
# mid alpha, : 0.65, mean = 4.76
# mid-high alpha: 0.75, mean = 4.83
# mid-high alpha: 0.80, mean = 4.775
# high alpha: 0.90, mean = 4.85
# near-full learn: 0.95, mean = 4.805
# very near full learn: 0.99, mean = 4.755
# full learning: 1.00, mean = 4.785

alphas <- c(0.0, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
            0.45, 0.5, 0.6, 0.65, 0.75, 0.8, 0.9, 0.95, 0.99, 1.0)

means <- c(0.98, 2.54, 2.59, 2.66, 3.34, 3.81, 4.115, 4.405, 4.51, 4.62, 4.56, 
           4.675, 4.655, 4.73, 4.76, 4.83, 4.775, 4.85, 4.805, 4.755, 4.785)
plot(alphas, means)

# Changing gamma, well-tuned parameters otherwise
# M = 1000, alpha = 0.75, gamma = 0.25. 