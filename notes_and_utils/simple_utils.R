library(distr)

###### contents of the scaffold

suppressPackageStartupMessages(library(distr))

## Utilities to make the distr library a bit nicer to use
weight <- 1
p <- function(distribution, realization) {
  d(distribution)(realization) # return the PMF or density 
}

Bern = function(probability_to_get_one) {
  DiscreteDistribution(supp = 0:1, prob = c(1-probability_to_get_one, probability_to_get_one))
}

## Key functions called by simPPLe programs

# Use simulate(distribution) for unobserved random variables
simulate <- function(distribution) {
  r(distribution)(1) # sample once from the given distribution
}

# Use observe(realization, distribution) for observed random variables
observe = function(realization, distribution) {
  # `<<-` lets us modify variables that live in the global scope from inside a function
  weight <<- weight * p(distribution, realization) 
}

###### solution

posterior = function(ppl_function, number_of_iterations) {
  numerator = 0.0
  denominator = 0.0
  for (i in 1:number_of_iterations) {
    weight <<- 1.0
    g_i = ppl_function()
    numerator = numerator + weight * g_i
    denominator = denominator + weight
  }
  return(numerator/denominator)
}


posterior_particles = function(ppl_function, number_of_iterations) {
  weight <<- 1.0
  dimension = length(ppl_function()) 
  samples = matrix(0, nrow = number_of_iterations, ncol = dimension)
  weights = rep(0, number_of_iterations)
  for (i in 1:number_of_iterations) {
    weight <<- 1.0       # reset the weight accumulator
    sample = ppl_function()
    samples[i,] = sample
    weights[i]  = weight
  }
  return(list(samples=samples, weights=weights))
}

ess = function(particles){
  w = particles$weights
  return(effective_sample_size(w))
}

effective_sample_size = function(w){
  (sum(w)^2)/sum(w^2)
}

representative_sample = function(snis_output, percentile=0.9999){
  ess = effective_sample_size(snis_output$weights)
  idx_ordered_weights = order(snis_output$weights, decreasing = TRUE)
  acc_norm_weights = cumsum(snis_output$weights[idx_ordered_weights])/sum(snis_output$weights)
  reduced_sample_size = max(round(ess), max(which(acc_norm_weights < percentile)))
  idx_subset = idx_ordered_weights[1:reduced_sample_size]
  list(samples = snis_output$samples[idx_subset,], weights = snis_output$weights[idx_subset])
}
weighted_scatter_plot = function(
    snis_output, 
    base_color_hex = hcl.colors(1, palette = "viridis"),
    plot_options = list(xlab="Param 1", ylab="Param 2")
){
  base_color = col2rgb(base_color_hex)/255
  
  # find the subset with almost all the mass
  snis_subset = representative_sample(snis_output)
  
  # linear transform of weights to [0,1]
  extreme_weights = range(snis_subset$weights, na.rm = T)
  alphas = (snis_subset$weights-extreme_weights[1])/diff(extreme_weights)
  
  # create colors with transparencies and plot
  points_color_alphas = rgb(base_color[1],base_color[2],base_color[3], alphas)
  call_args=c(
    list(x=snis_subset$samples[,1:2], col=points_color_alphas), 
    plot_options
  )
  do.call(plot, call_args)
}

posterior_particles = function(ppl_function, number_of_iterations) {
  weight <<- 1.0
  dimension = length(ppl_function()) 
  samples = matrix(0, nrow = number_of_iterations, ncol = dimension)
  weights = rep(0, number_of_iterations)
  for (i in 1:number_of_iterations) {
    weight <<- 1.0       # reset the weight accumulator
    sample = ppl_function()
    samples[i,] = sample
    weights[i]  = weight
  }
  return(list(samples=samples, weights=weights))
}

ess = function(particles){
  w = particles$weights
  return(effective_sample_size(w))
}

effective_sample_size = function(w){
  (sum(w)^2)/sum(w^2)
}

representative_sample = function(snis_output, percentile=0.9999){
  ess = effective_sample_size(snis_output$weights)
  idx_ordered_weights = order(snis_output$weights, decreasing = TRUE)
  acc_norm_weights = cumsum(snis_output$weights[idx_ordered_weights])/sum(snis_output$weights)
  reduced_sample_size = max(round(ess), max(which(acc_norm_weights < percentile)))
  idx_subset = idx_ordered_weights[1:reduced_sample_size]
  list(samples = snis_output$samples[idx_subset,], weights = snis_output$weights[idx_subset])
}
weighted_scatter_plot = function(
    snis_output, 
    base_color_hex = "#00cecb",
    plot_options = list(xlab="Param 1", ylab="Param 2")
){
  base_color = col2rgb(base_color_hex)/255
  
  # find the subset with almost all the mass
  snis_subset = representative_sample(snis_output)
  
  # linear transform of weights to [0,1]
  extreme_weights = range(snis_subset$weights, na.rm = T)
  alphas = (snis_subset$weights-extreme_weights[1])/diff(extreme_weights)
  
  # create colors with transparencies and plot
  points_color_alphas = rgb(base_color[1],base_color[2],base_color[3], alphas)
  call_args=c(
    list(x=snis_subset$samples[,1:2], col=points_color_alphas), 
    plot_options
  )
  do.call(plot, call_args)
}
