epsilon = 0.0
alpha = 0.0
gamma = 0.0
x = y = 1
library(pracma)
course = matrix(
  c(0, 1, 1,
    0, 1, 1,
    0, 0, 0), 
  nrow = 3, ncol = 3,
  byrow = T
)
order = matrix(
  c(5, 0, 0,
    4, 0, 0,
    3, 2, 1), 
  nrow = 3, ncol = 3,
  byrow = T
)
R = apply(order, MARGIN = c(1,2), 
      FUN = function(cell){
        runif(1, min = -1 + cell, max = cell)
      })
# pad for out of bounds movement
PAD <- matrix(-1, nrow = 5, ncol = 5)
PAD[2:4, 2:4] <- R
R <- PAD

set_start = function(x, y){
  course[x, y] <<- 2
}

tryCatch({
  cat("Selected action... \n")
  course[1, 4] = 2
  # perform epsilon-greedy and select action
  cat("Moving... \n")
  # attempt to update location from new movement
  # ...
}, error = function(e) {
  if(grepl("subscript out of bounds", e$message)) {
    cat("Agent attempted to move out of bounds. \n")
    # agent stays in the same location
    # assign static penalty (alternatively, expand R and index it)
  } 
}, finally = {
  cat("Updating Q(s,a)...")
})



