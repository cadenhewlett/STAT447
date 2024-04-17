library(ggplot2)
library(latex2exp)
library(hexbin)
library(scales)
library(knitr)
library(kableExtra)

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
###### Latex Table  ######
##########################

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

ggsave("final_project/data_raw.PNG", plot = p0, width = 6.5, height = 5)
