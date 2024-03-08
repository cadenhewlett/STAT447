epsilon = 0.2
alpha = 0.5
gamma = 0.5

x = y = x_old = y_old = 3

library(pracma)
library(extraDistr)
## Define the obstacle course
course = matrix(
  c(0, 1, 1,
    0, 1, 1,
    0, 0, 0), 
  nrow = 3, ncol = 3,
  byrow = T
)
## set the starting position
course[x, y] = 2

## set the Optimal Learning Envrionment (OLE) move order
order = matrix(
  c(5, 0, 0,
    4, 0, 0,
    3, 2, 1), 
  nrow = 3, ncol = 3,
  byrow = T
)

# Create an OLE Rewards Schema
R = apply(order, MARGIN = c(1,2), 
      FUN = function(cell){
        runif(1, min = -1 + cell, max = cell)
      })

# Create Pad for out of bounds movement
PAD <- matrix(-2, nrow = 5, ncol = 5)
PAD[2:4, 2:4] <- R
# Padded rewards matrix penalizes moving out of bounds
R <- PAD

# Define Action Set
A = list(
  UP <- function(){x <<- x - 1; },
  DOWN <- function(){x <<- x + 1; },
  RIGHT <- function(){y <<- y + 1;},
  LEFT <- function(){y <<- y - 1; }
)
VERBOSE = c("UP", "DOWN", "RIGHT", "LEFT")
# Initialize random uniform Q-Table
Q = array(runif(3*3*4, min = 0, max = 0.05), dim = c(3, 3, 4))
r_t = 0
# Attempt Movement at Time t
while( FALSE ) { #sum(c(x, y) == c(1, 1)) <= 2
tryCatch({
  # Epsilon-Greedy
  if(rbern(1, epsilon) == 1){
    print("Exploring...")
    a = rdunif(1, min = 1, max = 4)
  } else {
    print("Exploiting...")
    a = which.max( Q[x, y,] )
  }
  # Save Previous Position
  x_t = x; y_t = y;
  # Set old spot to zero
  course[x_t, y_t] = 0
  print(
    paste("Selected action:", VERBOSE[a] )
  )
  # perform epsilon-greedy and select action
  cat("Moving... \n")
  A[[a]]()
  print(paste("New Position:", x, y))
  # attempt to update location from new movement
  course[x, y] = 2
  # fetch reward
  r_t =  R[x,y]
  print(paste("Reward:", r_t))
}, error = function(e) {
  if(grepl("subscript out of bounds", e$message)) {
    cat("Agent attempted to move out of bounds. \n")
    # agent stays in the same location
    print(c(x, y))
    r_t <<- R[x, y]
    x <<- x_t
    y <<- y_t
    print(paste("New Position:",x, y))
    print(paste("Reward:", r_t))
  } 
}, finally = {
  course[x, y] = 2
  cat("Updating Q(x,y,a)...\n")
  print( paste("Old Q-Score:", Q[x_t, y_t, a]) )
  # Q-Table Update
  Q[x_t, y_t, a] = Q[x_t, y_t, a] + alpha*(
    r_t + gamma*Q[x, y, which.max( Q[x, y, ])] 
    - Q[x_t, y_t, a])
  print( paste("New Q-Score:", Q[x_t, y_t, a]) )
})
Sys.sleep(1)
}
course
# TODO: create offset schema for R indexing
R[4,2]
R
