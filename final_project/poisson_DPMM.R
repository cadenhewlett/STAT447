library(dirichletprocess)
# set seed for reproducibility
set.seed(447)
# start the clock
start_time = proc.time()
M = 10000
RUN = FALSE # TRUE if running sampler
TESTING = FALSE # TRUE if running synthetic validation

###########################
### Mixing Distribution ###
###########################

# define the framework conjugate mixture model
poisMd = MixingDistribution(
  distribution = "poisson",
  priorParameters = c(1, 2),
  conjugate = "conjugate"
)
# Part 1: Poisson Likelihood
Likelihood.poisson = function(mdobj, x, theta){
  return(as.numeric(dpois(x, theta[[1]])))
}
# Part 2: Gamma Prior : Base Measure
PriorDraw.poisson = function(mdobj, n){
  draws = rgamma(n, mdobj$priorParameters[1], mdobj$priorParameters[2])
  theta = list(array(draws, dim=c(1,1,n)))
  return(theta)
}
# Part 3: Posterior Draw (defined by conjugacy)
PosteriorDraw.poisson = function(mdobj, x, n=1){
  priorParameters = mdobj$priorParameters
  theta = rgamma(n,  priorParameters[1] + sum(x),
                     priorParameters[2] + nrow(x))
  return(list(array(theta, dim=c(1,1,n))))
}

# Part 4: Predictive Distribution by Marginalization
Predictive.poisson = function(mdobj, x){
  priorParameters = mdobj$priorParameters
  alpha = priorParameters[1]
  beta = priorParameters[2]
  pred = numeric(length(x))
  for(i in seq_along(x)){
    alphaP = alpha + x[i]
    betaP =  beta  + 1
    pred[i] = (beta ^ alpha) * gamma(alphaP) 
    pred[i] = pred[i] / ( (betaP^alphaP) * gamma(alpha) )
    pred[i] = pred[i] / prod(factorial(x[i]))
  }
  return(pred)
}

###########################
### D.P. Gibbs Sampling ###
###########################
?Predictive.normal
# read in cleaned data frame
df = read.csv("final_project/cleaned_crash_data.csv")
# monthly crash count, in 100s of crashes 
y = ( round((df$crash_count)/100) )

# create DP Poisson Mixture Model from mix dist. defined earlier
dirp = DirichletProcessCreate(y, poisMd)

if(RUN){
  # initialize and fit DPMM via Gibbs 
  dirp = Initialise(dirp)
  dirp = Fit(dirp, M) 
  dirp = Burn(dirp, 100)
  # compute, posterior frame: sampling from the posterior
  cat("Generating Posterior Frame...")
  # include 95% and 99% Credible Intervals
  postf = PosteriorFrame(dirp, 0:22, 10000, ci_size = c(0.1, 0.01))
  # save to avoid repeat simulation
  saveRDS(postf, file = "final_project/posterior_sampleframe.RDS")
  saveRDS(dirp, file =  "final_project/posterior_results.RDS")
}

###########################
## Synthetic Validation ###
###########################

if(TESTING){
  # generate three synthetic rates
  synth_data = c(rpois(269, lambda = 3),
                 rpois(538, lambda = 12),
                 rpois(269, lambda = 0.8))
  # initiate the dirichlett process poisson
  synth = DirichletProcessCreate(synth_data, poisMd)
  # initialize and fit DPMM via Gibbs 
  synth = Initialise(synth)
  synth = Fit(synth, M) 
  synth = Burn(synth, 100)
  # compute, posterior frame: sampling from the posterior
  cat("Generating Posterior Frame for Synthetic Data...")
  # include 95% and 99% Credible Intervals
  post_synth = PosteriorFrame(synth, 0:22, 10000, ci_size = c(0.1, 0.01))
  # save locally
  saveRDS(postf, file = "final_project/synthetic_sampleframe.RDS")
  saveRDS(dirp, file =  "final_project/synthetic_results.RDS")
}

# report runtime 
total_time = proc.time() - start_time
cat("Total Runtime of Script: ", total_time['elapsed'], "seconds\n")
