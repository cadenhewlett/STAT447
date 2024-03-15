(require(rstan))
#(require(magrittr))
(require(tidybayes))
(require(ggplot2))
(require(magrittr))
library(reshape2)
data = read.csv("data/vaccines_full.csv")
data$is_vaccinated = ifelse(data$arms == "vaccinated", 1, 0)
stan_converted = compose_data(data)
stan_converted

fit = stan(
  "hwk8q3.stan",
  seed = 1928,
  data = stan_converted,
  refresh = FALSE,
  iter = 10000,
  chains = 4
)


#fit %>% spread_draws(efficiencies[trials], prevalences[trials]) %>% head(5)
# 
# fit %>%
#   spread_draws(efficiencies[trials]) %>%
#   ggplot(aes(x = efficiencies, y = trials)) +
#   stat_halfeye() + 
#   theme_minimal()
data

# use spread_draws to get relevant values
df = fit %>% spread_draws(efficiencies[trials], prevalences[trials])
# melt the values into a data frame
plotDF = melt(data.frame(
  A =  df$efficiencies[df$trials == 1],
  P =  df$efficiencies[df$trials == 2],
  M =  df$efficiencies[df$trials == 3]
))
# greate density plots
plo  = ggplot(plotDF, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  xlim(0.35, 1) +  ylim(0, 22) +
  labs(
    title = "Density Plot of Posterior Vaccine Efficiencies",
    x = "Posterior Efficiency",
    y = "Density",
    subtitle = "Using the Hierarchical Model from Assignment 6"
  ) +
  theme_bw() +
  scale_fill_manual(
    name = "Vaccine:",
    values  =  c(
      "A" = "#2a9d8f",
      "P" = "#f4a261",
      "M" = "#264653"
    ),
    # get original names
    labels = unique(data$trial)
  ) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_line(color = "grey", linetype = "dotted"),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  facet_wrap( ~ variable, scales = "free_y", ncol = 1)
print(plo)
# ggplot(plotDF_long, aes(x = value, fill = variable)) +
#   geom_density(alpha = 0.5) +
#   xlim(0, 1) +  # Set x-axis limits
#   labs(title = "Density plot of vaccine efficiencies",
#        x = "Efficiency", y = "Density") +
#   scale_fill_manual(name = "Vaccine", values = vaccine_colors, labels = c("AZ Oxford", "Pfizer", "Moderna")) +
#   theme_minimal() +
#   theme(legend.position = "top",
#         panel.grid.major = element_line(color = "grey"),  # Major gridlines
#         panel.grid.minor = element_line(color = "grey", linetype = "dotted")  # Minor gridlines
#   ) +

samples <- rstan::extract(fit)

test = table( ( samples$is_moderna_better ) ) / length(samples$is_moderna_better)
?table

