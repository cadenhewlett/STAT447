epsilon = 0.50
alpha = 0.5
gamma = 0.15
M = 5000

x = y = x_old = y_old = 3

library(pracma)
library(extraDistr)
library(ggplot2)
library(tidyr)
library(abind)
## Define the obstacle course

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

# course = matrix(
#   c(0, 1, 1,
#     0, 1, 1,
#     0, 0, 0), 
#   nrow = 3, ncol = 3,
#   byrow = T
# )
course = matrix(
  c(0, 0, 0, 0,
    1, 1, 1, 0,
    1, 1, 0, 0),
  nrow = 3, ncol = 4,
  byrow = T
)
## set the starting position
course[x, y] = 2
course

## set the Optimal Learning Environment (OLE) move order

# order = matrix(
#   c(5, 0, 0,
#     4, 0, 0,
#     3, 2, 1), 
#   nrow = 3, ncol = 3,
#   byrow = T
# )
order = matrix(
  c(7, 6, 5, 4,
    0, 0, 0, 3,
    0, 0, 0, 2),
  nrow = 3, ncol = 4,
  byrow = T
)
# Create an OLE Rewards Schema
R = apply(order, MARGIN = c(1,2), 
          FUN = function(cell){
            if (cell == 0){
              runif(1, min = -2, max = -1)
            } else{
              # worst case possible is a tie
              cell + runif(1, min = -0.5, max = 0.5)
            }
          })


# Define Action Set
A = list(
  UP <- function(){x <<- x - 1; },
  DOWN <- function(){x <<- x + 1; },
  RIGHT <- function(){y <<- y + 1;},
  LEFT <- function(){y <<- y - 1; },
  STAY <- function(){}
)

VERBOSE = c("UP", "DOWN", "RIGHT", "LEFT", "STAY")
# Initialize random uniform Q-Table
Q = array(runif(dim(course)[1]*
                dim(course)[1]*
                length(A), min = 0, max = 0.05), 
          dim = c(dim(course)[1], 
                  dim(course)[2], 
                  length(A)))
r_t = 0
# Attempt Movement at Time t
iter = 0
DEBUG = FALSE

while( iter < ifelse(DEBUG, 10, M)) {
tryCatch({
  if(DEBUG){cat(paste("*** ROUND", iter+1, "***\n"))}
  # Epsilon-Greedy
  if(rbern(1, epsilon) == 1){
    if(DEBUG){ print("Exploring...") }
    a = rdunif(1, min = 1, max = 5)
  } else {
    if(DEBUG){ print("Exploiting...") }
    a = which.max( Q[x, y,] )
  }
  # Save Previous Position
  x_t = x; y_t = y;
  # Set old spot to zero
  course[x_t, y_t] = ifelse(y == 1 || x == 3, 0, 1)
  if(DEBUG){  print( paste("Selected action:", VERBOSE[a] ) ) }
  # perform epsilon-greedy and select action
  if(DEBUG){ cat("Moving... \n") }
  A[[a]]()
  if(DEBUG){ print(paste("New Position:", x, y)) }
  # attempt to update location from new movement
  course[x, y] = 2
  # fetch reward
  r_t =  R[x,y]
  # catch out of bounds
  if(length(course[x,y]) == 0){
    stop("subscript out of bounds")
  }
}, error = function(e) {
  if(grepl("subscript out of bounds", e$message)) {
    if(DEBUG){  cat("Agent attempted to move out of bounds. \n") }
    # agent stays in the same location
    r_t <<- -1
    x <<- x_t
    y <<- y_t
    if(DEBUG){ print(paste("New Position:",x, y)) }
  } 
}, finally = {
  course[x, y] = 2
  if(DEBUG){ cat("Updating Q(x,y,a)...\n") }
  if(DEBUG){ print( paste("Old Q-Score:", round(Q[x_t, y_t, a], 3) ) ) }
  # Q-Table Update
  Q[x_t, y_t, a] = Q[x_t, y_t, a] + alpha*(
    r_t + gamma*Q[x, y, which.max( Q[x, y, ])] 
    - Q[x_t, y_t, a])
  if(DEBUG){ print( paste("New Q-Score:", round(Q[x_t, y_t, a], 3) ) ) }
})
if(DEBUG){ print(course) }
iter = iter + 1
#print(paste("Reward:", round(r_t, 3)))
if(DEBUG){ Sys.sleep(0.5) }
}



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
    labs(title = "Panel of Heatmaps for Each Action Index, M = 1000",
         x = "Row Dimension",
         y = "Column Dimension",
         fill = "Value")
  print(p)
}

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

print(paste("The Predicted Route is:",
    paste(
    VERBOSE[ which.max( Q[3, 3, ] ) ],
    VERBOSE[ which.max( Q[3, 4, ] ) ],
    VERBOSE[ which.max( Q[2, 4, ] ) ],
    VERBOSE[ which.max( Q[1, 4, ] ) ],
    VERBOSE[ which.max( Q[1, 3, ] ) ],
    VERBOSE[ which.max( Q[1, 2, ] ) ],
    VERBOSE[ which.max( Q[1, 1, ] ) ],
    sep = ", ")))

P_QTB = abind(Q, R, along = 3)
dim(P_QTB)
VPLOT = c("UP", "DOWN", "RIGHT", "LEFT", "STAY", "ORIGINAL COURSE")
rawdf <- expand.grid(X = 1:dim(P_QTB)[1], 
                     Y = 1:dim(P_QTB)[2], 
                     Z = 1:dim(P_QTB)[3])
rawdf
values <- as.vector(P_QTB)
df <- cbind(rawdf, Value = values)

my_labeller <- as_labeller(function(z_index) VPLOT[as.numeric(z_index)])

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
P_QTB[, , 6] <- 0
R

## IDEA ##
## 
## FROM THIS, WE CAN ACTUALLY FIND A BAYESIAN MODEL OF 
## P(Q-TABLE IS WELL-TUNED | OUTCOMES)
## SO, RATHER THAN TUNE THE PARAMETERS, WE TAKE THE PARAMETERS
## AS THEY ARE (OR IN A RANDOM COMBO), AND FIND P(TUNED | ROUTE, START )
## THEN, FOR A GIVEN ROUTE, WE KNOW IN A ZERO-ONE STYLE WHETHER OR NOT
## IT IS TUNED. I.E. P(ROUTE | TUNED, START) IS ALWAYS 0% OR 100%
## THEN, A MORE INTERESTING APPROACH COULD BE IF THE TRUE OPTIMAL POLICY
## IS UNKOWN. THEN, P(ROUTE | TUNED, START) IS SOME FUNCTION OF THE FINAL
## REWARD (WHICH WE WOULD HAVE TO PUT LIKELIHOODS ON)
## 


## Could also monitor convergence of algorithm by placing priors, etc. 
## On the Q-Table. And then compare it with a random uniform table.
## Then, the observed difference of that table from the random uniform 
## Is the probability of learned action.


## Alternatively, could treat this as the "Frequentist" method.
## And implement a bayesian one
## Bayesian Version: 
## Each cell is a variable X_1, ... X_n
## From each cell, there are actions available
## Action ~ categroical({a1, a2, a3, a4}, {p1, p2, p3, p4})
## Alternatively, it has a 1/4 chance of going any direction
## And a Bernoulli trial to stay (maybe)
## However, each direction has a probability associated with it
## and these are changed over time from the reward states
## this would require thinky-times but it would basically be
## P(Go Left | X1), updated by P(Go Left | {Reward, X1})
## However this would have serial dependence and be a DTMC with 12 interconnected states
## Which is slightly complicated.
## But may be more performant
## It would be cool to compare iteration requirements


P_QTB
length(A)
P_QTB[, , 6]

rawdf

# TODO: Path from any given start point.
# for (x in 1:3){
#   for(y in 1:4){
#     print(
#       paste("If the Agent was in postition ", 
#             paste(c(x, y), collapse = ", "),
#             " it would go ",
#       VERBOSE[which.max(Q[x, y, ])],
#       ".", sep = ""))
#   }
# }


#which.max(Q[1, 2, ])
