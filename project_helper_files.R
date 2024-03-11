# test python translate fp matrix reading
getwd()
df = read.csv("data/floor_plan_matrix1.csv", header = FALSE)
colnames(df) <- NULL
df_mat = as.matrix(df)
head(df_mat)



# creates a heatmap plot of the q table
plot_Q_table <- function(Q){
  VERBOSE = c("UP", "DOWN", "RIGHT", "LEFT", "STAY")
  rawdf <- expand.grid(X = 1:dim(Q)[1], Y = 1:dim(Q)[2], Z = 1:dim(Q)[3])
  values <- as.vector(Q)
  df <- cbind(rawdf, Value = values)
  
  my_labeller <- as_labeller(function(z_index) VERBOSE[as.numeric(z_index)])
  
  p = ggplot(df, aes(x = X, y = Y, fill = Value)) +
    geom_tile() + 
    scale_fill_gradient(low = "white", high = "red") +
    facet_wrap(~ Z, ncol = 2, labeller = my_labeller) +
    theme_minimal() +
    labs(title = "Panel of Heatmaps for Each Action Index",
         x = "Row Dimension",
         y = "Column Dimension",
         fill = "Value")
  print(p)
}

# Counts the success rate of the optimal policy
# then we theorize this is binomial(n, p)
# where p is some "as yet known" function of the hyperparams
# this would give (I think) P(Results | Hyper Parameters)
assess_performance <- function(Q_table){
  sum(
    which.max( Q_table[3, 3, ] ) == 4,
    which.max( Q_table[3, 2, ] ) == 4,
    which.max( Q_table[3, 1, ] ) == 1,
    which.max( Q_table[2, 1, ] ) == 1,
    which.max( Q_table[1, 1, ] ) == 5
  )
}
