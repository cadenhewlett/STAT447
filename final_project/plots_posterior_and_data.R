library(ggplot2)
# What we effectively have is discrete weekly counts (in 100s), with 
# latent categorization (severity) that is masked
# however, with a DP Infinite Mixture Model we 
# can still compute a posteriori estimates of the rate parameter 
# (despite the fact that the count distribution is multimodal)
## How to plot 
pf = readRDS("final_project/posterior_sampleframe.RDS")
print(ggplot() +
   geom_ribbon(data=pf,
                 aes(x=x, ymin=X5., ymax=X95.),
                 colour=NA,
                 fill="red",
                 alpha=0.2) + #credible intervals
   geom_line(data=pf, aes(x=x, y=Mean), colour="red") + theme_bw() )#+ #mean
