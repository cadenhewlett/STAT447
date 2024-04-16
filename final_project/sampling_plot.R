library(dirichletprocess)
library(latex2exp)
library(logspline)
library(ggplot2)
library(scales)
library(pbapply)

##########################
##### Posterior Plot #####
##########################

set.seed(447)
# true if running simulations
RUN = FALSE
# read in DP Fit and Data
df = read.csv("final_project/cleaned_crash_data.csv")
dp = readRDS(file =  "final_project/posterior_results.RDS")
# get the rates in the clusters
rates = unlist(dp$clusterParameters)
# get the weights and cluster sizes
sizes = unlist(dp$pointsPerCluster)
weights = unlist (dp$weights)
# declare number of simulations
M = 10000
N = nrow(df)
# run M simulations of size N from the posterior mixture distribution
if (RUN) {
  cat("Generating posterior draws... \n")
  # then, simulate M draws from the posterior DPMM
  simulated_datasets =
    pbsapply(1:M,
             function(m) {
               # where each draw is of size n = 1076
               # mixed by rates and cluster sizes
               unlist(sapply(1:length(sizes), function(i) {
                 # generate z_i draws at rate r_i
                 rpois(sizes[i], rates[i])
               }))
             })
  cat("Done! \n")
  cat("Generating posterior densities... \n")
  # then compute the densities to be compared against the data histogram
  simulations = matrix(0, nrow = 24, ncol = M)
  # here, we get the proportion counts
  simulations = (pbsapply(1:M,
                          function(m) {
                            as.numeric(table(factor(simulated_datasets[, m], levels = 0:23)))
                          }))
  # save locally (reduces re-run time)
  saveRDS(simulations, file = "final_project/posterior_sims.RDS")
  cat("Done! \n")
} else {
  cat("Using saved simulations... \n")
  simulations = readRDS(file = "final_project/posterior_sims.RDS")
}
# density profile comparison
posterior_data <- data.frame(
  x = 0:23,
  avg_post = apply(simulations, MARGIN = 1, mean),
  low_post = apply(simulations, MARGIN = 1, quantile, probs = 0.025),
  hi_post  = apply(simulations, MARGIN = 1, quantile, probs = 0.975)
)
p1 <- ggplot() +
  geom_bar(
    data = df,
    aes(x = round(crash_count / 100),
        y = after_stat(prop)),
    stat = "count",
    fill = "gray",
    color = "white",
    alpha = 0.5
  ) +
  labs(
    title = TeX(
      "Estimated Posterior PDF for Rate Parameter $\\lambda$"
    ),
    subtitle = TeX(
      "Posterior Mean and 95% Credible Interval with Histogram of Observed Data"
    ),
    y = "Probability Density",
    x = TeX("Count Values")
  ) +
  theme_bw() +
  geom_line(
    data = posterior_data,
    aes(x = x, y = avg_post/N),
    color = "#10a19d",
    size = 0.8
  ) +
  geom_ribbon(
    data = posterior_data,
    aes(x = x, ymin = low_post/N, ymax = hi_post/N),
    fill = "#10a19d",
    alpha = 0.2
  ) +
  scale_y_continuous(n.breaks = 10) +
  theme(
    panel.grid.minor = element_line(
      color = "grey90",
      linetype = "dashed",
      linewidth = 0.5
    )
  )

print(p1)
ggsave("final_project/post_comp.png", plot = p1, width = 7, height = 5)


# avg_post
# # ECDF 
# counts = as.numeric(table(round(df$crash_count/100)))
# counts = c(counts, rep(0, 24-length(counts)))
# 
# # 
# #    geom_step(aes(x = cumsum(counts)
# # , cumsum(counts/N), color = "Observed"))
# 
# ggplot(data.frame(x = round(df$crash_count / 100)), aes(x = x)) +
#   stat_ecdf(geom = "step") +
#   ggtitle("ECDF of Observed Data") +
#   xlab("Data values") +
#   ylab("ECDF") +
#   theme_bw() 
# ecdf_data( ecdf(simulations[, 1]) )
