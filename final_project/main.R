library(extraDistr)
library(ggplot2)
library(latex2exp)
library(RColorBrewer)
library(scales)
set.seed(1928) #1928
# base distributions, 
G_0_nor <- function(n) { rnorm(n, 0, 4) }
# G_0_dir <- function(n) { rdirichlet(n, c(1, 1, 1, 1)) } 
# clusters 
K <- 1000
# generate stick breaking finite approximation
b <- rbeta(K, 1, 8) 
# empty vector for pulls
p <- numeric(K)
# break 1
p[1] <- b[1]
# further breaks
p[2:K] <- sapply(2:K, function(i) b[i] * prod(1 - b[1:(i-1)]))

# create vector
y <- G_0_nor(K) # cbind(G_0_nor(n),  G_0_dir(n)) 
# index sample according to stick-break probabilities
sampled <- sample(1:K, prob = p, replace = TRUE)
# this creates theta_i
theta = y[sampled]


# then just plot stuff
palette_4 <- c("#0b7875", "#006466", "#065a60", "#0b525b", "#144552",
                   "#1b3a4b", "#212f45", "#272640", "#312244", "#3e1f47", 
                   "#4d194d") 
palette_3 = c("#fcf2d7", "#FFBA08", "#FAA307", "#F48C06",
               "#E85D04", "#DC2F02", "#D00000",
               "#9D0208", "#6A040F", "#370617", "#03071E")
palette_2 <- c("#d8f3dc", "#b7e4c7", "#a6ddb8", "#95d5b2", "#74c69d", "#52b788", 
               "#49a47a", "#40916c", "#2d6a4f", "#1b4332", "#081c15")
palette_1 <- c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", 
                   "#34a0a4", "#168aad", "#1a759f", "#1e6091", "#184e77", 
                   "#15395d")
# + scale_fill_manual(values = rev(palette_4))
plotDF = data.frame(  DirB = theta,  DirP = log(p))
# plot heatmap of results
p1 = ggplot(plotDF, aes(x = DirB, y = DirP)) +
  geom_density_2d_filled() +
  labs(
    title =
      TeX("Finite Approximation of Dirichlet Process DP($\\alpha G_0$)"),
    subtitle = TeX("Where $K = 1000$ and $G_0 \\sim N(0, \\sigma^2)$"),
    y = TeX("Log of Mixture Weights: $\\{\\pi_k\\}_{k = 1}^K$"),
    x = TeX("Cluster Parameters: $\\{\\theta_k\\}_{k = 1}^K$")
  )  + theme_bw()  +  scale_fill_viridis_d(option = "plasma") + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(margin = margin(b = -3.5, unit = "pt")),
    plot.subtitle = element_text(margin = margin(b = -5, unit = "pt")),
    legend.position = "right",
    legend.title = element_blank(),
    axis.ticks.length = unit(-2, "mm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 0, unit = "mm", l = -5)
  ) + scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))
print(p1) #trbl

ggsave("final_project/dirch_appx.png", plot = p1, width = 7, height = 5)
