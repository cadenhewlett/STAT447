course_A = matrix(
  c(0, 1, 1,
    0, 1, 1,
    0, 0, 0), 
  nrow = 3, ncol = 3,
  byrow = T
)

course_B = matrix(
  c(0, 0, 0, 0,
    1, 1, 1, 0,
    1, 1, 0, 0),
  nrow = 3, ncol = 4,
  byrow = T
)

course_C = matrix(
  c(0, 0, 1, 
    1, 0, 1,
    1, 0, 0,
    1, 1, 1),
  nrow = 4, ncol = 3,
  byrow = T
)

course_D = matrix(
  c(0, 1, 1, 1, 1,
    0, 1, 1, 1, 1,
    0, 1, 0, 0, 1,
    0, 1, 1, 0, 1,
    0, 0, 0, 0, 1),
  nrow = 5, ncol = 5,
  byrow = T
)

course_X <- matrix(
  c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1), nrow = 10, byrow = TRUE)
solution_X <- matrix(
  c(21, 20, 19, 18, 17,  0,  0,  0,  0,  0,
    0,  0,  0,  0, 16,  0,  0,  0,  0,  0,
    0,  0,  1,  0, 15,  0,  0,  0,  0,  0,
    0,  0,  2,  0, 14,  0,  0,  0,  0,  0,
    0,  0,  3,  0, 13,  0,  0,  0,  0,  0,
    0,  0,  4,  0, 12,  0,  0,  0,  0,  0,
    0,  0,  5,  0, 11,  0,  0,  0,  0,  0,
    0,  0,  6,  0, 10,  0,  0,  0,  0,  0,
    0,  0,  7,  8,  9,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0), nrow = 10, byrow = TRUE)

# Define the function to create a heatmap from a matrix
plot_course_heatmap <- function(matrix_data) {
  matrix_data_long <- as.data.frame(as.table(as.matrix(matrix_data)))
  
  names(matrix_data_long) <- c("Row", "Column", "Value")
  
  ggplot(matrix_data_long, aes(x = Row, y = Column, fill = Value)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() +
    labs(x = "Row", y = "Column", fill = "Value")
}


# Create the heatmap
print( plot_course_heatmap(course_X) )


