(require(rstan))
#(require(magrittr))
(require(tidybayes))
(require(ggplot2))
(require(magrittr))

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
fit  %<>% recover_types(data)

fit

fit %>% spread_draws(efficiencies[trials], prevalences[trials]) %>% head(5)
# 
# fit %>%
#   spread_draws(efficiencies[trials]) %>%
#   ggplot(aes(x = efficiencies, y = trials)) +
#   stat_halfeye() + 
#   theme_minimal()


df = fit %>% spread_draws(efficiencies[trials], prevalences[trials])
par(mfrow = c(1, 3))
hist( df$efficiencies[df$trials == 1], xlim = c(0, 1))
hist( df$efficiencies[df$trials == 2], xlim = c(0, 1) )
hist( df$efficiencies[df$trials == 3], xlim = c(0, 1) )

print(
  sum( df$efficiencies[df$trials == 2] > df$efficiencies[df$trials == 3]) / 
     length(df$efficiencies[df$trials == 2]) )
