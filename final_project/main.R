library(extraDistr)
library(ggplot2)


# base distributions, 
G_0_nor <- function(n) { rnorm(n, 0, 4) }
G_0_dir <- function(n) { rdirichlet(n, c(1, 1, 1, 1)) } 
# clusters 
n <- 1000
# generate stick breaking finite approximation
b <- rbeta(n, 1, 10) 
# empty vector for pulls
p <- numeric(n)
# break 1
p[1] <- b[1]
# further breaks
p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))

# create vector
y <- cbind(G_0_nor(n),  G_0_dir(n)) 
# index sample according to stick-break probabilities
sampled <- sample(1:n, prob = p, replace = TRUE)
# this creates theta_i
theta = y[sampled,]


# take log-product of dirichlett component so that it can be visualized
plotDF = data.frame( B = theta[,1],  
                     P = log(apply(theta[, 2:5], 1, prod)))

# plot heatmap of results
plot = ggplot(plotDF, aes(x = B, y = P)) +
  geom_density_2d_filled() +
  labs(title = "Dirichlet Process",
       y = "Log-Product Dirichlet Base",
       x = "Normal Base") + theme_bw()

print(plot)

unique(plotDF)
# DP generates the 2D-distribution P, where one dimension of P is
# normal N(m, s) and the other dimension is dirchlet

# then, x_{i,j} are sampled from the dirichlet dimension of P 
# and, betas are sampled from the normal dimension of p
# all according to the probability mass defined by p (i.e. stick-breaking)
# then, Y_i | (all of that) is the poisson fit of the i-th data point
# by the indicator-based regression equation provided?