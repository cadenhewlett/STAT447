#require(rstan, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(rstan, quietly = TRUE)
require(knitr, quietly = TRUE)
df = (read.csv(url("https://github.com/UBC-Stat-ML/web447/raw/0d6eaa346d78abe4cd125e8fc688c9074d6331d9/data/hubble-1.csv")) %>%
        rename(distance = R..Mpc.) %>%
        rename(velocity = v..km.sec.))

df = read.csv("data/hubble.csv")[1:24, c(3:4)]
colnames(df) = c("distance", "velocity")
velocity = df$velocity/1000
distance = df$distance

# get the data frame size
N_obs = nrow(df)

# hubble_predict <- stan_model(
#   file = "hwk8q1.stan",
#   model_name = "hubble_predict", 
#   verbose = TRUE
# )
CrI <- function(index){
  one_out = stan(
    "hwk8q1.stan",
    model_name = "hubble_predict",
    seed = 1990,
    data = list(
      N  = N_obs-1,
      d = df$distance[-index], 
      v = df$velocity[-index], 
      x_pred = df$distance[index]
    ),
    refresh = FALSE
  )
  info = extract(one_out)
  return(c(
    quantile(info$y_pred, 0.10),
    quantile(info$y_pred, 0.90)
  ))
}


all_loo = sapply(1:N_obs, function(n){CrI(n)})

ci_limits = matrix(all_loo, nrow = nrow(df), ncol = 2, byrow = T)
ci_limits

merged_df = df %>%
  bind_cols(data.frame(CI_L = ci_limits[, 1], CI_R = ci_limits[, 2])) %>%
  mutate(Inside_CI = (velocity >= CI_L & velocity <= CI_R))
merged_df %>%
  ggplot(aes(
    x = 1:N_obs,
    y = velocity,
    ymin = CI_L,
    ymax = CI_R,
    color = Inside_CI
  )) +
  geom_point() +
  geom_errorbar(alpha = 0.6) +
  scale_color_manual(values = c("#ed6809", "#166e4f"),
                     labels = c("No", "Yes")) +
  theme_bw() +
  labs(
    x = "Point",
    y = "Velocity",
    title = "Leave-One-Out 80% Credible Intervals",
    subtitle = paste((sum(merged_df$Inside_CI)/N_obs)*100, 
                     "% of Intervals Contain the True Value", sep = "")
  )



