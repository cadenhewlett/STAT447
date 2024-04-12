library(extraDistr)
library(ggplot2)
library(latex2exp)
library(RColorBrewer)
library(scales)
# seed for reproducibility
set.seed(447) 
# base distributions, 
G_0 <- function(n) { rgamma(n, 1, 1) }
# clusters 
K <- 1000
# generate stick breaking finite approximation
b <- rbeta(K, 1, 10) # 2.1 is other nice example
# empty vector for pulls
p <- numeric(length = K)
# initial stick break
p[1] <- b[1]
# further breaks
p[2:K] <- sapply(2:K, function(i) b[i] * prod(1 - b[1:(i-1)]))
# create vector
y <- G_0(K) 
# index sample according to stick-break probabilities
sampled <- sample(1:K, prob = p, replace = TRUE)
# this creates theta_i
theta = y[sampled]


# then just plot stuff
palette_1 <- c("#D9ED92", "#BDE68D", "#A6DE8C", "#8ED48E" ,
               "#73C794", "#58B999", "#40A8A0", "#2898A7", "#1787AB",
               "#1A77A0", "#1D6695", "#1B5885",
               "#174971" ,"#15395D")

# +  scale_fill_viridis_d(option = "plasma") 
plotDF = data.frame(  DirB = theta,  DirP = log(p))
# plot heatmap of results
p1 = ggplot(plotDF, aes(x = DirB, y = DirP)) +
  geom_density_2d_filled() +
  labs(
    title =
      TeX("Finite Approximation of Dirichlet Process : DP($\\alpha G_0$) Realization"),
    subtitle = TeX("Where $K = 1000$, $G_0 \\sim$ gamma(1, 1) and $\\alpha = 10$"),
    y = TeX("Log of Mixture Weights: $\\{\\pi_k\\}_{k = 1}^K$"),
    x = TeX("Cluster Parameters: $\\{\\lambda_k\\}_{k = 1}^K$")
  )  + theme_bw()  +  scale_fill_manual(values = rev(palette_1)) + theme(
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
#plot(plotDF$DirB, plotDF$DirP)
# palette_2 <- c( "#0B7875","#036969", "#035F63", "#08585E",
#                 "#0C515A", "#134753", "#183E4E", "#1D3649", 
#                 "#222E44", "#272740", "#2E2343", "#372145",
#                 "#411E48", "#4D194D")