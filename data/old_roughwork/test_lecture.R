source("scaffold.R")
source("simple.R")
library(extraDistr)
library(distr)
df = read.csv("hubble.csv")
dist = df[3]
velocity = df[4]
dist = dist[!is.na(dist)]
velocity = velocity[!is.na(velocity)]
plot(dist, velocity)

df = df[ 1:(nrow(df)-1), ]
df$distance = dist
df$velocity = velocity/1000

# source("../../solutions/simple.R")
# source("../blocks/simple_utils.R")

regression = function() {
  sigma = simulate(Exp(10))
  slope = simulate(Norm(0, 1))
  for (i in 1:nrow(df)) { 
    # observe our data
    distance = df[i, "distance"]
    velocity = df[i, "velocity"]
    # update model assumptions
    observe(velocity, Norm(distance*slope, sigma))
  }
  return(c(slope, sigma))
}


post = posterior(regression, 100)
post
#osterior = posterior_particles(regression, 1000)

# weighted_scatter_plot(posterior, plot_options = list(xlab="slope parameter", ylab="sd parameter"))
# 
# plot(df$distance, df$velocity, xlab = "distance", ylab = "velocity")
# 
# xs = seq(0, 2, 0.01)
# samples = posterior$samples 
# norm_weights = posterior$weights / sum(posterior$weights)
# 
# for (i in 1:nrow(samples)) {
#   slope     = samples[i, 1]
#   pr = norm_weights[i]
#   lines(xs, slope * xs, col = rgb(red = 0, green = 0, blue = 0, alpha = pr*20))
# }
# library("ggplot2")
# posterior.as.df <- data.frame(slopes = samples[,1], norm_weights)
# ggplot(posterior.as.df, aes(x = slopes, weight = norm_weights)) + 
#   geom_histogram(binwidth = 0.02) + 
#   xlim(0.2, 0.6) +
#   xlab("slope parameter") + 
#   ylab("probability") +
#   theme_minimal()
