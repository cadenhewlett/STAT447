# test python translate fp matrix reading
getwd()

library(ggplot2)
library(reshape2)
df = read.csv("data/floor_plan_matrix1.csv", header = FALSE)
colnames(df) <- NULL
df_mat = as.matrix(df)

matrix_data_long <- melt(df_mat)
colnames(matrix_data_long) <- c("X1", "X2", "value")

mat_plot = ggplot(matrix_data_long, aes(x=X2, y=rev(X1), fill=factor(value))) + 
  geom_tile(color="white") + 
  scale_fill_manual(values=c("0"="white", "1"="black")) + 
  theme_minimal() + 
  ggtitle("Viable Location Information Matrix from Floor Plan") +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks=element_blank(), axis.title.x=element_blank(), 
        axis.title.y=element_blank(), legend.position="none") + 
  coord_fixed()
print(mat_plot)


# initial ideas for rewards system


# Assuming 'df_mat' is your matrix and start_point is c(start_x, start_y)
start_point <- c(5, 5)  # Example start point, adjust as needed

# Step 2 & 3: Calculate Euclidean distance and adjust values for 0 cells
for (i in 1:nrow(df_mat)) {
  for (j in 1:ncol(df_mat)) {
    if (df_mat[i, j] == 0) {
      dist <- sqrt((i - start_point[1])^2 + (j - start_point[2])^2)
      df_mat[i, j] <- -(dist^(1.2)) # Use negative distance to indicate it decreases away from start
    }
  }
}

# Step 4: Replace 1 cells with random negative numbers
df_mat[df_mat == 1] <- runif(sum(df_mat == 1), min=-100, max=-99)  # Adjust range as needed

# Prepare data for ggplot
df_mat_long <- melt(as.matrix(df_mat))
names(df_mat_long) <- c("X", "Y", "value")

# Step 5: Use ggplot to plot a heatmap
pgrid = ggplot(df_mat_long, aes(Y, rev(X), fill=value)) +
  geom_tile() + ggtitle("Euclidean Reward with Penalized Walls") +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), legend.title = element_blank()) +
  coord_fixed()
print(pgrid)
