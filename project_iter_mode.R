testing = F
if (testing){
  results = sapply(1:200, function(iter){
    source("project_outline.R")
  })
  
  print( mean( unlist( results[1, ] ) ) )
}
par(mfrow = c(2, 2))
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
epsilons = c(0.00, 0.01, 0.02, 0.05, 0.10, 0.15, 
                 0.20, 0.25, 0.30, 0.35, 0.40, 0.80, 1.00)
performance = c(1.66, 2.63, 3.11, 4.31, 4.56, 4.70, 4.81, 4.80, 4.84,
                4.92, 4.93, 5, 5)

plot((epsilons), performance, main = "Epsilon")
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
plot((alphas), means, main = "Alpha")

# Changing gamma, well-tuned parameters otherwise
# M = 1000, alpha = 0.75, epsilon = 0.20

# no gamma: 0.00, mean = 4.735
# near-no gamma, 0.01, mean = 4.79
# low gamma, 0.02, mean = 4.805
# low gamma, 0.10, mean = 4.785
# mid-low gamma, 0.15, mean = 4.815
# mid-low gamma, 0.25, mean = 4.825
# mid gamma, 0.35, mean = 4.7
# mid gamma, 0.50: mean = 4.7
# mid gamma, 0.60: mean = 4.685
# mid gamma, 0.70: mean = 4.655
# mid-high gamma: 0.80, mean = 4.545
# high gamma, 0.85, mean = 4.505
# high gamma, 0.90, mean = 4.37
# very high gamma, 0.95, mean = 4.095
# very high gamma, 0.97, mean = 3.805
# very high gamma, 0.99, mean = 3.035
# near full future, 0.995, mean = 2.51
# full future: 1.00, mean = 2.28

gammas <- c(0.0, 0.01, 0.02, 0.10, 0.15, 0.25, 0.35, 0.50, 0.60, 0.70, 
            0.80, 0.85, 0.90, 0.95, 0.97, 0.99, 0.995, 1.00)

gmeans <- c(4.735, 4.79, 4.805, 4.785, 4.815, 4.825, 4.7, 4.7, 4.685, 4.655,
           4.545, 4.505, 4.37, 4.095, 3.805, 3.035, 2.51, 2.28)

plot(gammas, (gmeans), main = "Gamma")



# Changing N_iter, well-tuned parameters otherwise

# no repetitions
# gamma = 0.25,  alpha = 0.75, epsilon = 0.20
# M = 1, mean = 0.995
# M = 10, mean = 1.53
# M = 25, mean = 2.045
# M = 50, mean = 2.325
# M = 100, mean = 2.91
# M = 150, mean = 3.655
# M = 200, mean = 3.655
# M = 250, mean = 4.165
# M = 500, mean = 4.655
# M = 1000, mean = 4.755
# M = 2500, mean = 4.865
# M = 5000, mean = 4.945

Ms <- c(1, 10, 25, 50, 100, 150, 200, 250, 500, 1000, 2500, 5000)
Mmeans <- c(0.995, 1.53, 2.045, 2.325, 2.91, 3.655, 3.655, 4.165, 4.655, 4.755, 4.865, 4.945)

plot((Ms), (Mmeans), main = "M")

# perhaps the likelihood is an exponential linear combination of epsilon, alpha, inv(gamma) and M