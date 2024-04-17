library(ggplot2)
library(latex2exp)
library(RColorBrewer)
library(scales)
# seed for reproducibility
set.seed(1924)
# number of clusters
K = 20
# base measure/distribution
G_0 = function(n) {
  rgamma(n, 1, 2)
}
alpha = rgamma(1, 1, 1)

#############################
##### Finite D.P. Appx. #####
#############################


# generate stick breaking finite approximation
b <- rbeta(K, 1, alpha)
# empty vector for pulls
p <- numeric(length = K)
# initial stick break
p[1] <- b[1]
# further breaks following GEM(a) definition from methods
p[2:K] <- sapply(2:K, function(i)
  b[i] * prod(1 - b[1:(i - 1)]))
# then, sample from base distribution by weight probabilities
# this creates the finite approximation as discussed in the methods
theta <- sample(G_0(K), prob = p, replace = TRUE)

#############################
##### FINITE D.P. PLOT ######
#############################


plotDF = data.frame(DirB = theta,  DirP = log(p))
# plot heatmap of results
p1 = ggplot(plotDF, aes(x = DirB, y = DirP)) +
  geom_density_2d_filled() +
  labs(
    title =
      TeX(
        "Finite Approximation of Dirichlet Process : DP($\\alpha G_0$) Realization"
      ),
    subtitle = TeX(
      "Where $K = 20$, $G_0 \\sim$ gamma(1, 2) and $\\alpha \\sim$ gamma(1,1)"
    ),
    y = TeX("Log of Mixture Weights: $\\{\\pi_k\\}_{k = 1}^K$"),
    x = TeX("Cluster Parameters: $\\{\\lambda_k\\}_{k = 1}^K$")
  )  + theme_bw() + scale_fill_viridis_d(option = "magma")  + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(margin = margin(b = -3.5, unit = "pt")),
    plot.subtitle = element_text(margin = margin(b = -5, unit = "pt")),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks.length = unit(-2, "mm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 0, unit = "mm", l = -5)
  ) + scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10)

print(p1) # trbl
ggsave(
  "final_project/dirch_appx.png",
  plot = p1,
  width = 7,
  height = 5
)
