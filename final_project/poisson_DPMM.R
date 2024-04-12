library(dirichletprocess)
library(ggplot2)
# set seed for reproducibility
set.seed(447)

# define the framework conjugate mixture model
poisMd <- MixingDistribution(
  distribution = "poisson",
  priorParameters = c(1, 1),
   conjugate = "conjugate"
)

# Part 1: Poisson Likelihood
Likelihood.poisson <- function(mdobj, x, theta){
  return(as.numeric(dpois(x, theta[[1]])))
}
# Part 2: Gamma Prior : Base Measure
PriorDraw.poisson <- function(mdobj, n){
  draws <- rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
  theta <- list(array(draws, dim=c(1,1,n)))
  return(theta)
}
# Part 3: Posterior Draw (defined by conjugacy)
PosteriorDraw.poisson <- function(mdobj, x, n=1){
  priorParameters <- mdobj$priorParameters
  theta <- rgamma(n, priorParameters[1] + sum(x),
                     priorParameters[2] + nrow(x))
  return(list(array(theta, dim=c(1,1,n))))
}

# Part 4: Predictive Distribution by Marginalization
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

# read in cleaned data frame
df = read.csv("final_project/cleaned_crash_data.csv")

# monthly crash count, in 100s of crashes 
y = ( round((df$crash_count)/100) )

# create DP Poisson Mixture from MD defined earlier
dp <- DirichletProcessCreate(y, poisMd)
# initialize and fit DPMM via MCMC
dp <- Initialise(dp)
dp <- Fit(dp, 10000)
# compute, posterior frame: sampling from the posterior
cat("Generating Posterior Frame...")
pf <- PosteriorFrame(dp, 0:50, 10000)

# posterior weights and lambdas (will be formatted into a table)
## note that the model correctly idenitfies three distinct rates
data.frame(Weights=dp$weights, lambda=c(dp$clusterParameters[[1]]))

# save to avoid repeat simulation
saveRDS(pf, file = "posterior_sampleframe.RDS")
saveRDS(dp, file = "raw_dirichlett_process.RDS")



