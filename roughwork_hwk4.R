launches = c(1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1)
library(latex2exp)
source("scaffold.R")
source("simple.R")
source("simple_utils.R")
?hcl.colors
?hcl.pals()
logistic_regression = function() {
  beta_1 = simulate(Norm(0, 1))
  beta_0 = simulate(Norm(0, 1))
  sapply(1:length(launches),
           function(L){
             observe(launches[L],
                     Bern(plogis(beta_0 + beta_1*L)))
           })

  return(c(beta_0, beta_1, weight))
}

# posterior(logistic_regression, 1000)

posterior = posterior_particles(logistic_regression, 1000)
weighted_scatter_plot(posterior,
                      plot_options =
                        list(
                          xlab = TeX(r'($\hat{\beta}_0, Intercept \; Terms$)'),
                          ylab = TeX(r'($\hat{\beta}_1, Slope \; Terms$)'),
                          main = TeX(r'(Weighted Forward Simulated Values for m\in\{1, 2, ..., M\})')
                        ))


# for (L in 1:length(launches)){
#   #print(plogis(beta_0 + beta_1*L))
#   observe(launches[L],
#           Bern(plogis(beta_0 + beta_1*L)))
# }