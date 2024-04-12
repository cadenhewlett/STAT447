library(dirichletprocess)
library(ggplot2)
# browseVignettes(package = "dirichletprocess")
dp <- DirichletProcessGaussian(rnorm(10))

# custom conjugate mixture model
poisMd <- MixingDistribution(
  distribution = "poisson",
  priorParameters = c(1, 1),
   conjugate = "conjugate"
)

# Poisson Likelihood
Likelihood.poisson <- function(mdobj, x, theta){
  return(as.numeric(dpois(x, theta[[1]])))
}
# Gamma Prior
PriorDraw.poisson <- function(mdobj, n){
  draws <- rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
  theta <- list(array(draws, dim=c(1,1,n)))
  return(theta)
}
# A draw from the posterior by conjugacy
PosteriorDraw.poisson <- function(mdobj, x, n=1){
  priorParameters <- mdobj$priorParameters
  theta <- rgamma(n, priorParameters[1] + sum(x),
                     priorParameters[2] + nrow(x))
  return(list(array(theta, dim=c(1,1,n))))
}
# Predictive Distribution by maginalization
Predictive.poisson <- function(mdobj, x){
  priorParameters <- mdobj$priorParameters
  pred <- numeric(length(x))
  for(i in seq_along(x)){
    alphaPost <- priorParameters[1] + x[i]
    betaPost <- priorParameters[2] + 1
    pred[i] <- (priorParameters[2] ^ priorParameters[1]) / gamma(priorParameters[1])
    pred[i] <- pred[i] * gamma(alphaPost) / (betaPost^alphaPost)
    pred[i] <- pred[i] * (1 / prod(factorial(x[i])))
  }
  return(pred)
}

# What we effectively have is discrete monthly counts, with 
# latent categorization (severity) that is masked
# however, with a DP Infinite Mixture Model we 
# can still compute a posteriori estimates of the rate parameter 
# (despite the fact that the count distribution is multimodal)
df = read.csv("final_project/cleaned_crash_data.csv")
# monthly crash count, in 100s of crashes 
y = ( round((df$crash_count)/100) )
dp <- DirichletProcessCreate(y, poisMd)
dp <- Initialise(dp)
dp <- Fit(dp, 10000)
## Posterior Frame allows sampling from the posterior
cat("Generating Posterior Frame...")
pf <- PosteriorFrame(dp, 0:50, 10000)

## note that the model correctly idenitfies three distinct rates
data.frame(Weights=dp$weights, lambda=c(dp$clusterParameters[[1]]))

## Save to avoid repeat simulation
saveRDS(pf, file = "posterior_sampleframe.RDS")
saveRDS(dp, file = "raw_dirichlett_process.RDS")
## How to plot 
# print(ggplot() +
#    geom_ribbon(data=pf,
#                  aes(x=x, ymin=X5., ymax=X95.),
#                  colour=NA,
#                  fill="red",
#                  alpha=0.2) + #credible intervals
#    geom_line(data=pf, aes(x=x, y=Mean), colour="red") + theme_bw() )#+ #mean
