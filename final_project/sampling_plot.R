library(dirichletprocess)
library(latex2exp)
library(logspline)
library(ggplot2)
library(scales)
library(pbapply)

##########################
#### Posterior Plots #####
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
B = 100 # burn in 
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
               unlist(pbsapply(1:length(sizes), function(i) {
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

##########################
## Pedictive vs. Actual ##
##########################

# density profile comparison
posterior_data <- data.frame(
  x = 0:23,
  avg_post = apply(simulations, MARGIN = 1, mean),
  low_post = apply(simulations, MARGIN = 1, quantile, probs = 0.025),
  hi_post  = apply(simulations, MARGIN = 1, quantile, probs = 0.975),
  vlow_post = apply(simulations, MARGIN = 1, quantile, probs = 0.005),
  vhi_post  = apply(simulations, MARGIN = 1, quantile, probs = 0.995)
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
      "Mean Estimated Posterior Predictive for DPMM"
    ),
    subtitle = TeX(
      "95% and 99% Credible Interval with Histogram of Observed Data"
    ),
    y = "Probability Density",
    x = TeX("Count Values")
  ) +
  theme_bw() +
  geom_line(
    data = posterior_data,
    aes(x = x, y = avg_post/N),
    color = "#075957",
    size = 0.8
  ) +
  xlim(-1, 20) +
  geom_ribbon(
    data = posterior_data,
    aes(x = x, ymin = low_post/N, ymax = hi_post/N),
    fill = "#075957",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = posterior_data,
    aes(x = x, ymin = vlow_post/N, ymax = vhi_post/N),
    fill = "#10a19d",
    alpha = 0.20
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

##########################
#### Posterior Rates #####
##########################

# Etract MCMC iterations post Burn-In for each rate param
L_data = data.frame(sapply(1:3, function(L) {
  sapply(1:(M - B), function(m) {
    (dp$clusterParametersChain)[[m]][[1]][L]
  })
}))

colnames(L_data) = c("Lambda 1", "Lambda 2", "Lambda 3")

L_data_long = L_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = -id,
    names_to = "Parameter",
    values_to = "Rate"
  )
p2 = ggplot(L_data_long,
       aes(
         x = Parameter,
         y = Rate,
         fill = Parameter,
         colour = Parameter
       )) +
  geom_boxplot(width = 0.2, alpha = 0.5,
               outlier.shape = 3, outlier.size = 0.5) +
  geom_violin(trim = TRUE, alpha = 0.3) +
  scale_fill_manual(values = c(
    "Lambda 1" = "#414833",
    "Lambda 2" = "#936639",
    "Lambda 3" = "#c2c5aa"
  )) +
  scale_colour_manual(values = c(
    "Lambda 1" = "#333d29",
    "Lambda 2" = "#7f4f24",
    "Lambda 3" = "#656d4a"
  )) +
  labs(
    title = TeX("Boxplots of Posterior Rate Parameters"),
    subtitle = TeX("For Dominant-Populated Clusters $i \\in \\{1, 2, 3\\}$"),
    x = "Parameter",
    y = "Posterior Rate"
  ) +
  scale_x_discrete(labels = c(
    TeX("$\\lambda_1$"),
    TeX("$\\lambda_2$"),
    TeX("$\\lambda_3$")
  )) +
  scale_y_continuous(n.breaks = 10)+
  theme_bw() +  theme(
    panel.grid.minor = element_line(
      color = "grey90",
      linetype = "dashed",
      linewidth = 0.5
    ),
    legend.position = "none",
    axis.text.x = element_text(size = 12)
  )
#print(p2)

ggsave("final_project/post_box.png", plot = p2, width = 4, height = 6)


##########################
#### Cluster Reports #####
##########################

cluster_density = data.frame( table(sapply(dp$weightsChain, length) ))

probs = sapply(1:nrow(cluster_density),
       function(c){cluster_density$Freq[c]/(sum(cluster_density$Freq))})
# values used in paragraph
sum( seq(from = 3, to = 13)*probs )
final_density = data.frame(table(sapply(dp$weightsChain[9400:9900], length)))
final_probs = sapply(1:nrow(final_density),
               function(c){final_density$Freq[c]/(sum(final_density$Freq))})
final_probs
first_density = data.frame(table(sapply(dp$weightsChain[0:500], length)))
first_probs = sapply(1:nrow(first_density),
                     function(c){first_density$Freq[c]/(sum(first_density$Freq))})
first_probs
