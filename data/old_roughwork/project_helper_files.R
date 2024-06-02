# test python translate fp matrix reading
getwd()
# state space sizes
c(22*45, 52*52, 20*20)
library(ggplot2)
library(reshape2)
df = read.csv("data/floor_plan_matrix2.csv", header = FALSE)
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
start_point <- c(30,5)
fuzzy = FALSE
for (i in 1:nrow(df_mat)) {
  for (j in 1:ncol(df_mat)) {
    # calcualte euclidean distance for each cell
    if (df_mat[i, j] == 0) {
      dist <- sqrt((i - start_point[1])^2 + (j - start_point[2])^2)
      # normal pull with mean proporional to negative distance
      if(fuzzy){
        df_mat[i, j] <- rnorm(1, mean = -(dist^(1.2)), sd = 1)
      } # or static pull
      else{
        df_mat[i, j] <- -(dist^(1.1)) 
      }
    }
  }
}
df_mat[df_mat == 1] <- runif(sum(df_mat == 1), min=-100, max=-99)  
df_mat_long <- melt(as.matrix(df_mat))
names(df_mat_long) <- c("X", "Y", "value")
pgrid = ggplot(df_mat_long, aes(Y, rev(X), fill=value)) +
  geom_tile() + ggtitle("Euclidean Reward with Penalized Walls") +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(), legend.title = element_blank()) +
  coord_fixed()
print(pgrid)
