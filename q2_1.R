library(extraDistr)
library(ggplot2)
K = 20
prior_probabilities = sapply(0:K, function(k){(k / K)*(1 - k/K)})
plot_data <- data.frame(
  value = 0:K,
  probability = prior_probabilities
)
# plot prior
p <- ggplot(plot_data, aes(x = value, y = 0, yend = probability)) +
  geom_segment(aes(xend = value), color = "#b298dc", linewidth = 0.75) +
  geom_point(aes(x = value, y = probability), color = "#6f2dbd") +
  labs(x = "Value", y = "Probability") +
  ggtitle("Plot of Prior Probabilities") + theme_bw() +
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))


realizations = sapply(0:K, function(k){k/K})


# to get the prior probabilities, we need gamma(x) and the normalizing constant Z

# 
likelihood = (realizations)^3

gamma = prior_probabilities*likelihood

pi = gamma/sum(gamma)


plot_data_2 <- data.frame(
  value = 0:K,
  probability = pi
)
# plot posterior
p2 <- ggplot(plot_data_2, aes(x = value, y = 0, yend = probability)) +
  geom_segment(aes(xend = value), color = "#4361ee", linewidth = 0.75) +
  geom_point(aes(x = value, y = probability), color = "#3a0ca3") +
  labs(x = "Value", y = "Posterior Probability",
       title = "Plot of Posterior Probabilities",
       subtitle = "Given the Three Successful Rocket Launches") + theme_bw() +
  theme(panel.grid.minor = element_line(
    color = "grey90",
    linetype = "dashed",
    linewidth = 0.5
  ))
print(p2)

get_posterior_mean <- function(){
  return(sum(realizations*pi))
}
get_posterior_mean()
(0:K)[which.max(pi)]
