require(rstan)

fit = stan(
  seed = 123,
  file = "hwk7q1.stan",  # Stan program
  data = list(n=3, k=3),        # named list of data
  iter = 1000                   # number of samples to draw
)

print(fit)

model = extract(fit)
median(model$p)
library(ggplot2)




ggplot(data.frame(model$p), aes(x = model.p)) +
  geom_histogram(
    bins = 30,
    fill = "#B7EFC5",
    color = "#10451D",
    alpha = 0.7
  ) +
  geom_vline(aes(xintercept = mean_val, color = "Mean"),
             linetype = "dashed",
             linewidth = 1) +
  geom_vline(aes(xintercept = median_val, color = "Median"),
             linetype = "dashed",
             linewidth = 1) +
  scale_color_manual(
    name = "",
    values = c("Mean" = "#8B2FC9", 
               "Median" = "#4A0A77"),
    labels = c(paste("Mean =", round(mean_val, 2)),
               paste("Median =", round(median_val, 2)))
  ) +
  labs(
    title = "Histogram of Posterior Probability of Beta-Binomial Model",
    subtitle = "Given k = 3, n = 3",
    x = "Posterior Probability",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_line(colour = "gray", linetype = "dotted")
  ) +
  guides(color = guide_legend(override.aes = 
                    list(linetype = c("dashed", "dashed"))))


