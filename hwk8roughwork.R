require(rstan, quietly = TRUE)
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(dplyr))

df = (read.csv(url("https://github.com/UBC-Stat-ML/web447/raw/0d6eaa346d78abe4cd125e8fc688c9074d6331d9/data/hubble-1.csv")) %>%
        rename(distance = R..Mpc.) %>%
        rename(velocity = v..km.sec.))

df = read.csv("data/hubble.csv")[1:24, c(3:4)]
colnames(df) = c("distance", "velocity")
velocity = df$velocity/1000
distance = df$distance

# get the data frame size
N_obs = nrow(df)
N_train
# 
# reg_fit = stan(
#   seed = 1990, 
#   file = "hwk8q1.stan", 
#   data = list(N = length(distance), 
#               d = distance,
#               v = velocity),        
#   iter = 2000                   
# )


hubble_predict <- stan_model(
  file = "hwk8q1.stan",
  model_name = "hubble_predict", 
  verbose = FALSE
)

fit = stan(
  "hwk8q1.stan",
  model_name = "hubble_predict",
  seed = 1990,
  data = list(
    N  = N_obs-1,
    d = df$distance[-N_obs], 
    v = df$velocity[-N_obs], 
    x_pred = df$distance[N_obs]
  )
)

fit
