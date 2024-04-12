library(dirichletprocess)
library(ggplot2)
# browseVignettes(package = "dirichletprocess")
dp <- DirichletProcessGaussian(rnorm(10))
dp$mixingDistribution

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


# What we effectively have is discrete yearly counts, with 
# latent categorization (province and condition) that is masked to the reporter.
# however, with a DP Infinite Mixture Model we 
# can still compute a posteriori estimates of the rate parameter 
# (despite the fact that the count distribution is multimodal)


# dp <- DirichletProcessCreate(yTest, poisMd)
# dp <- Initialise(dp)
# dp <- Fit(dp, 1000, progressBar = TRUE)
df = read.csv("final_project/cleaned_crash_data.csv")
# monthly crash count, in 100s of crashes 
y = ( round((df$crash_count)/100) )
dp <- DirichletProcessCreate(y, poisMd)
dp <- Initialise(dp)
dp <- Fit(dp, 10000)
cat("Generating Posterior Frame...")
pf <- PosteriorFrame(dp, 1:50, 2000)
# ?PosteriorFrame
length(df$crash_count)
# trueFrame <- data.frame(x= 0:20,
#                         y= 70/(150+70 )*dpois(0:20, 3) + 150/(150+70 )*dpois(0:20, 10))
# (y)
print(ggplot() +
   geom_ribbon(data=pf,
                 aes(x=x, ymin=X5., ymax=X95.),
                 colour=NA,
                 fill="red",
                 alpha=0.2) + #credible intervals
   geom_line(data=pf, aes(x=x, y=Mean), colour="red") + theme_bw() )#+ #mean
# rpois(1000, 7)
 #  geom_line(data=trueFrame, aes(x=x, y=y)) #true

# test = ( data.frame(Weight=dp$weights, Theta=unlist(c(dp$clusterParameters))) )
# test$Theta[which.max(test$Weight)]
# tail(test$Weight)
# xGrid <- seq(0, 1, by=0.01)
# 
# postEval <- replicate(100, PosteriorFunction(dp)(xGrid))
# meanFrame <- data.frame(Mean=rowMeans(postEval), x=xGrid)
# plot(meanFrame)
# quantileFrame <- data.frame(x=xGrid, t(apply(postEval, 1, quantile, prob=c(0.03, 0.97))))
# trueFrame <- data.frame(x=xGrid, y=(0.5*dpois(xGrid, 10)+0.5*dpois(xGrid, 0.1)))
# 
# ggplot()  + geom_ribbon(data=quantileFrame, aes(x=x, ymin=X3., ymax=X97.), alpha=0.4) + 
#   geom_line(data=meanFrame, aes(x=x, y=Mean, colour="Posterior Mean")) + geom_line(data=trueFrame, aes(x=x, y=y, colour="True"))