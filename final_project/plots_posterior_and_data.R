library(ggplot2)
library(latex2exp)
library(hexbin)
library(scales)

##########################
##### Raw Data Plot ######
##########################

# read in data
data = read.csv("final_project/cleaned_crash_data.csv")
# dividing to account for aggregation
y = (round(data$crash_count / 100))
ind = seq_along(y)

p0 = ggplot(data.frame(ind, y), aes(x = ind, y = y)) +
  geom_hex(alpha = 1) +
  scale_fill_gradient(low = "#e8e1df", high = "#5e1317",
                      name = "Point Density") +
  theme_bw() +
  labs(
    title = "Hex Plot of Weekly-Aggregated Car Accidents in Chicago by Logged Time",
    subtitle = "From 2014-2023, Missing Values Imputed by Nearest-Neighbours",
    x = "Time Logged in System",
    y = "Car Crashes (100s of Crashes)"
  ) +  theme(
    panel.grid.minor = element_line(
      color = "grey90",
      linetype = "dashed",
      linewidth = 0.5
    ),
    legend.margin = margin(0, 0, 0, 0),
    legend.justification = "top"
  )
print(p0)

##########################
#### Posterior Rates #####
##########################

library(knitr)
library(kableExtra)
# read in results from other file
dirichlet_results = readRDS("final_project/posterior_results.RDS")
# format nicely
results_DF = data.frame(
  lambdas = unlist(dirichlet_results$clusterParameters),
  weights = dirichlet_results$weights )
# make a table - columns are clusters
results = kable(t(round(results_DF, 3)),
                format = "latex",
                booktabs = TRUE,
                caption = "DPMM Posterior Parameters and Weights") %>%
  kable_styling(latex_options = "striped", position = "center") %>%
  column_spec(1, bold = TRUE, border_left = TRUE) 
# write to latex to render 
# this gives the initial design, which I edited later
writeLines(results,  "final_project/results.tex")

##########################
### Poisson Regression ###
##########################

# fit poisson regression
freq_model = glm(y~seq_along(y),  family = poisson(link = "log"))
coefs = data.frame(summary(freq_model)$coefficients)
# extract coefficients
b0 = coefs$Estimate[1] ; b1 = coefs$Estimate[2]
# we use Z quantiles due to asymptotic normality (n = 1076, very large)
b0U = b0 +  qnorm(0.995)*coefs$Std..Error[1]
b1U = b1 +  qnorm(0.995)*coefs$Std..Error[2]
# lower estimates
b0L = b0 -  qnorm(0.995)*coefs$Std..Error[1]
b1L = b1 -  qnorm(0.995)*coefs$Std..Error[2]
# compute lambda
lambda = exp(b0 + b1*ind)
# and confidence interval
lambdaL = exp(b0L + b1L*ind)
lambdaU = exp(b0U + b1U*ind)
# figure out rate plot from model
mle  = sapply(0:22, function(x) {mean(dpois(x, lambda) )})
mleL = sapply(0:22, function(x) {mean(dpois(x, lambdaL) )})
mleU = sapply(0:22, function(x) {mean(dpois(x, lambdaU) )})

# 99% confidence interval by MLE
lower = ifelse(mleL < mleU, mleL, mleU)
upper = ifelse(mleL >= mleU, mleL, mleU)


##########################
##### Posterior Plot #####
##########################

pf = readRDS("final_project/posterior_sampleframe.RDS")

colnames(pf) = c("Mean", "Q5", "Q05", "Q95", "Q995", "xVal")

p1 = ggplot(pf, aes(x = xVal, y = Mean)) +
  geom_ribbon(
    data = pf,
    aes(x = xVal, ymin = Q05, ymax = Q995),
    fill = "#10a19d",
    alpha = 0.15
  ) +
  geom_line(aes(colour = "DPMM Posterior")) + theme_bw() +
  geom_line(aes(x = 0:22, y = mle, colour = "Regression MLE")) +
  geom_ribbon(aes(x = xVal, ymin = lower, ymax = upper),
              fill = "#765b82",
              alpha = 0.1) +
  labs(
    title = TeX(
      "Posterior Distribution for Rate Parameter $\\lambda$ Using Mean of 10,000 Draws"
    ),
    subtitle = TeX(
      "Comparison with Estimated $\\lambda_i$ from Frequentist Poisson Regression (MLE)"
    ),
    y = TeX("Posterior Value of   $\\lambda$"),
    x = TeX("Value of $x \\in [0, 22]$")
  ) + scale_color_manual(values = c(
    "DPMM Posterior" = "#10a19d",
    "Regression MLE" = "#765b82"
  )) +
  guides(color = guide_legend(override.aes = list(linewidth = 1))) +
  theme(
    panel.grid.minor = element_line(
      color = "grey90",
      linetype = "dashed",
      linewidth = 0.5
    ),
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(-3, 0, -3, 0)
  )

print(p1)




ggsave("final_project/data_raw.PNG", plot = p0, width = 6.5, height = 5)
ggsave("final_project/post_comp.png", plot = p1, width = 7, height = 5)


# What we effectively have is discrete weekly counts (in 100s), with 
# latent categorization (severity) that is masked
# however, with a DP Infinite Mixture Model we 
# can still compute a posteriori estimates of the rate parameter 
# (despite the fact that the count distribution is multimodal)
## How to plot : runtime  694.59 s