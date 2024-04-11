require(dplyr)
require(knitr)
require(ggplot2)
require(dirichletprocess)
require(rstan)
require(tidyr)

rats 

alpha0 <- 0.01
beta0 <- 0.01

alphaPosterior <- alpha0 + sum(rats$y) 
betaPosterior <- beta0 + sum(rats$N) - sum(rats$y)

wk <- c(0.5, 0.5)
phik_alpha <- c(2, 5)
phik_beta <- c(3, 10)

xGrid <- seq(0, 1, by=0.01)

frameF <- data.frame(x=xGrid, 
                     y1 = wk[1] * dbeta(xGrid, phik_alpha[1], phik_beta[1]),
                     y2 = wk[2] * dbeta(xGrid, phik_alpha[2], phik_beta[2]))

FIT <- FALSE # assume fit = T
its <- 1500

if(FIT){
  
  print("Starting Fit")
  
  thetaDirichlet <- rbeta(nrow(rats), alphaPosterior, betaPosterior)
  
  dp <- DirichletProcessBeta(thetaDirichlet,
                             1, 
                             mhStep = c(0.002, 0.005),
                             alphaPrior = c(2, 0.5))
  
  dp$numberClusters <- nrow(rats)
  dp$clusterLabels <- seq_len(nrow(rats))
  dp$clusterParameters <- PriorDraw(dp$mixingDistribution, nrow(rats))
  dp$pointsPerCluster <- rep_len(1, nrow(rats))
  
  dp <- Fit(dp, 1)
  
  postFuncEval <- matrix(ncol=its, nrow=length(xGrid))
  muPostVals <- matrix(ncol=its, nrow=nrow(rats))
  nuPostVals <- matrix(ncol=its, nrow=nrow(rats))
  
  pb <- txtProgressBar(max=its, width=50, char="-", style=3)
  
  for(i in seq_len(its)){
    
    postClusters <- PosteriorClusters(dp)
    
    postFuncEval[,i] <- PosteriorFunction(dp)(xGrid)
    
    wk <- sample.int(length(postClusters$weights), 
                     nrow(rats), 
                     replace = T, 
                     prob = postClusters$weights)
    
    muPost <- postClusters$params[[1]][,,wk]
    nuPost <- postClusters$params[[2]][,,wk]
    
    aPost <- muPost * nuPost
    bPost <- (1-muPost) * nuPost
    
    muPostVals[,i]  <- muPost
    nuPostVals[,i]  <- nuPost
    
    newTheta <- rbeta(nrow(rats), aPost + rats$y, bPost + rats$N - rats$y)
    
    dp <- ChangeObservations(dp, newTheta)
    dp <- Fit(dp, 100, updatePrior = T, progressBar = F)
    
    setTxtProgressBar(pb, i)
    
  }

  saveList <- list()
  saveList$muPostVals <- muPostVals
  saveList$nuPostVals <- nuPostVals
  saveList$postFuncEval <- postFuncEval

 saveRDS(saveList, "dpPrior.RDS")
} else {
  print("Fit from Cache")
  saveList <- readRDS("dpPrior.RDS")
  saveList$muPostVals -> muPostVals
  saveList$nuPostVals -> nuPostVals
  saveList$postFuncEval -> postFuncEval
}


dirichletParamsMu <- data.frame(Value = c(muPostVals[,-(1:its/2)]))
dirichletParamsMu$Parameter= "Mu"

dirichletParamsNu <- data.frame(Value = c(nuPostVals[,-(1:its/2)]))
dirichletParamsNu$Parameter="Nu"

dirichletParams <- bind_rows(dirichletParamsMu, dirichletParamsNu)


ggplot(dirichletParams, aes(x=Value)) + geom_density() + facet_wrap(~Parameter, scales="free")


dirEvalFrame <- data.frame(x=xGrid,
                           Mean = rowMeans(postFuncEval),
                           LQ = apply(postFuncEval, 1, quantile, probs = 0.025),
                           UQ = apply(postFuncEval, 1, quantile, probs = 1 - 0.025),
                           Model = "Dirichlet")
