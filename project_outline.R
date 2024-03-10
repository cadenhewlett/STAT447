epsilon = 0.20
alpha = 0.75
gamma = 0.75

x = y = x_old = y_old = 3

library(pracma)
library(extraDistr)
library(ggplot2)
library(tidyr)

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
course

## set the Optimal Learning Environment (OLE) move order
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

# # Create Pad for out of bounds movement
# PAD <- matrix(-2, nrow = 5, ncol = 5)
# PAD[2:4, 2:4] <- R
# # Padded rewards matrix penalizes moving out of bounds
# R <- PAD

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
iter = 0
while( iter < 50) { #sum(c(x, y) == c(1, 1)) <= 2
tryCatch({
  # Epsilon-Greedy
  if(rbern(1, epsilon) == 1){
#    print("Exploring...")
    a = rdunif(1, min = 1, max = 4)
  } else {
#    print("Exploiting...")
    a = which.max( Q[x, y,] )
  }
  # Save Previous Position
  x_t = x; y_t = y;
  # Set old spot to zero
  course[x_t, y_t] = ifelse(y == 1 || x == 3, 0, 1)
#  print( paste("Selected action:", VERBOSE[a] ) )
  # perform epsilon-greedy and select action
#  cat("Moving... \n")
  A[[a]]()
#  print(paste("New Position:", x, y))
  # attempt to update location from new movement
  course[x, y] = 2
  # fetch reward
  r_t =  R[x,y]
  # 
  if(length(course[x,y]) == 0){
    stop("subscript out of bounds")
  }
}, error = function(e) {
  if(grepl("subscript out of bounds", e$message)) {
#    cat("Agent attempted to move out of bounds. \n")
    # agent stays in the same location
#    print(c(x, y))
    r_t <<- -1
    x <<- x_t
    y <<- y_t
#    print(paste("New Position:",x, y))
#    print(paste("Reward:", r_t))
  } 
}, finally = {
  course[x, y] = 2
#  cat("Updating Q(x,y,a)...\n")
#  print( paste("Old Q-Score:", round(Q[x_t, y_t, a], 3) ) )
  # Q-Table Update
  Q[x_t, y_t, a] = Q[x_t, y_t, a] + alpha*(
    r_t + gamma*Q[x, y, which.max( Q[x, y, ])] 
    - Q[x_t, y_t, a])
#  print( paste("New Q-Score:", round(Q[x_t, y_t, a], 3) ) )
})
#print(course)
iter = iter + 1
#print(paste("Reward:", round(r_t, 3)))
if(x == 1 & y == 1){print(paste(
  "Target Reached in", iter, "steps."));  break}
#Sys.sleep(0.5)
}
#Q[3, 3, ]
VERBOSE[ which.max( Q[3, 3, ] ) ]
VERBOSE[ which.max( Q[3, 2, ] ) ]
VERBOSE[ which.max( Q[3, 1, ] ) ]
VERBOSE[ which.max( Q[2, 1, ] ) ]

Q


## Plotting the Q-Table
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
R
