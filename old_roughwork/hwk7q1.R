# require(rstan)
# 
# fit = stan(
#   seed = 123,
#   file = "hwk7q1.stan",  # Stan program
#   data = list(n=3, k=3),        # named list of data
#   iter = 1000                   # number of samples to draw
# )
# 
# print(fit)
# 
# model = extract(fit)
# median(model$p)
# library(ggplot2)
# 
# 
# 
# 
# ggplot(data.frame(model$p), aes(x = model.p)) +
#   geom_histogram(
#     bins = 30,
#     fill = "#B7EFC5",
#     color = "#10451D",
#     alpha = 0.7
#   ) +
#   geom_vline(aes(xintercept = mean_val, color = "Mean"),
#              linetype = "dashed",
#              linewidth = 1) +
#   geom_vline(aes(xintercept = median_val, color = "Median"),
#              linetype = "dashed",
#              linewidth = 1) +
#   scale_color_manual(
#     name = "",
#     values = c("Mean" = "#8B2FC9", 
#                "Median" = "#4A0A77"),
#     labels = c(paste("Mean =", round(model$p, 2)),
#                paste("Median =", round(model$p, 2)))
#   ) +
#   labs(
#     title = "Histogram of Posterior Probability of Beta-Binomial Model",
#     subtitle = "Given k = 3, n = 3",
#     x = "Posterior Probability",
#     y = "Frequency"
#   ) +
#   theme_bw() +
#   theme(
#     legend.position = "top",
#     panel.grid.minor = element_line(colour = "gray", linetype = "dotted")
#   ) +
#   guides(color = guide_legend(override.aes = 
#                     list(linetype = c("dashed", "dashed"))))


suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(dplyr))

df = (read.csv(url("https://github.com/UBC-Stat-ML/web447/raw/0d6eaa346d78abe4cd125e8fc688c9074d6331d9/data/hubble-1.csv")) %>%
  rename(distance = R..Mpc.) %>%
  rename(velocity = v..km.sec.))

df = read.csv("hubble.csv")[1:24, c(3:4)]
colnames(df) = c("distance", "velocity")
velocity = df$velocity/1000
distance = df$distance

reg_fit = stan(
  seed = 1990, 
  file = "hwk7q2.stan", 
  data = list(N = length(distance), 
              d = distance,
              v = velocity),        
  iter = 2000                   
)

reg_vals = extract(reg_fit)
qL = quantile(reg_vals$beta, 0.025)
qU = quantile(reg_vals$beta, 0.975)
xbar = mean(reg_vals$beta)
data.frame(reg_vals)
p2 <- ggplot(data.frame(reg_vals), aes(x = beta)) +
  geom_histogram(bins = 30,
                 fill = "#F9EAE1",
                 color = "#7D4F50") +
  geom_segment(
    size = 1,
    linetype = 'dashed',
    aes(
      x = xbar,
      xend = xbar,
      y = 0,
      yend = Inf,
      color = "Mean"
    ),
  ) +
  geom_segment(
    size = 1,
    linetype = 'dashed',
    aes(
      x = qL,
      xend = qL,
      y = 0,
      yend = Inf,
      color = "95% Cr.I."
    )
  ) +
  geom_segment(
    size = 1,
    linetype = 'dashed',
    aes(
      x = qU,
      xend = qU,
      y = 0,
      yend = Inf,
      color = "95% Cr.I."
    )
  ) +
  scale_color_manual(
    name = "",
    values = c("Mean" = "#772F1A", "95% Cr.I." = "#F58549"),
    labels = c(
      paste(
        "95% Credible Interval = (",
        round(qL, 2),
        ", ",
        round(qU, 2),
        ")",
        sep = ""
      ),
      paste("Posterior Mean =", round(xbar, 2))
    )
  ) + theme_bw()  +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted")
  ) +
  labs(
    title = expression("Posterior Distribution of" ~ beta ~ "Given Data"),
    x = "Slope Values",
    y = "Frequency"
  ) +
  scale_x_continuous(breaks = seq(
    from = floor(min(reg_vals$beta)),
    to = ceiling(max(reg_vals$beta)),
    by = 0.01
  )) 

p2