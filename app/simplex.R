table <- read.csv("food.csv")
optimize <- function(sample_list) {
  
  sample_list <- c(21, 11, 12, 22, 29, 48, 44, 56, 38, 52, 58, 4, 6, 23, 28, 36, 5, 61, 62, 35)
  table <- table[sample_list,]
  # MINIMUM NUTRIENT MAXIMUM
  # 2000 Calories 2250
  # 0 Cholesterol 300
  # 0 Total Fat 65
  # 0 Sodium 2400
  # 0 Carbohydrates 300
  # 25 Dietary Fiber 100
  # 50 Protein 100
  # 5000 Vitamin A 50000
  # 50 Vitamin C 20000
  # 800 Calcium 1600
  # 10 Iron 30
  # Max Serving per food, 10
  
  RHS <- c(2000, 2250, 0, 300, 0, 65, 0, 2400, 0, 300, 25, 100, 50, 100, 5000, 50000, 50, 20000, 800, 1600, 10, 30)
  # nrow = number of constraints + objective function
  # ncol = slack variables (1 per constraint) + variables + RHS
  NUM_FOODS <- nrow(table)
  NUM_CONSTRAINTS <- 22 + NUM_FOODS
  tableu <- matrix(0, nrow = NUM_CONSTRAINTS + 1, ncol = NUM_FOODS + 1)
  
  # Setting up Constraints Matrix
  
  for (i in 1:22) {
    # Fill in coefficients of food types
    for (j in 1:NUM_FOODS) {
      tableu[i,j] <- table[j,(i+1)/2 + 3]
    }
    
    # Fill in RHS
    tableu[i,ncol(tableu)] <- RHS[i] 
    
    if (i %% 2 == 0) {
      # Negate maximization constraints
      tableu[i,] <- -tableu[i,]
    }
  }
  
  # Fill in serving constraints
  # 10 servings max per food
  for (i in 1:NUM_FOODS) {
    tableu[22 + i, i] <- -1
    tableu[22 + i, ncol(tableu)] <- -10
  }
  
  # Fill in the objective function
  for (i in 1:NUM_FOODS) {
    tableu[nrow(tableu),i] <- table[i,2]
  }
  
  tableu <- t(tableu)
  tableu[nrow(tableu),] <- -tableu[nrow(tableu),]
  
  # Setting up minimization table
  min_tableu <- matrix(0, nrow = nrow(tableu), ncol = ncol(tableu) + nrow(tableu))
  min_tableu[,1:ncol(tableu) - 1] <- tableu[,1:ncol(tableu) - 1]
  min_tableu[,ncol(min_tableu)] <- tableu[,ncol(tableu)]
  
  # Fill in slack variables
  for (i in 1:nrow(min_tableu)) {
    min_tableu[i,ncol(tableu) - 1 + i] <- 1
  }
  starting_min_tableu <- min_tableu
  # Solve for Simplex of min_tableu
  
  
  min_tableu <- simplex_method(min_tableu)
  servings <- round(min_tableu[nrow(min_tableu),(ncol(min_tableu) - NUM_FOODS - 1):ncol(min_tableu)],2)
  
  # Remove useless information
  servings[length(servings) - 1] <- servings[length(servings)]
  servings <- servings[1:(length(servings) - 2)]
  
  result <- matrix(c(table$Food, servings), nrow = NUM_FOODS)
  cost_per_food <- round(table$Price * servings[1:NUM_FOODS], 2)
  # Paste cost column to result
  result <- cbind(result, cost_per_food)
  result <- rbind(result, c("Total", sum(servings), sum(cost_per_food)))
  return(result)
  
}

simplex_method <- function(min_tableu){
  iterations <- 0
  MAX_ITERATIONS <- 1000
  while (min(min_tableu[nrow(min_tableu),]) < 0 && iterations < MAX_ITERATIONS) {
    # Find pivot column
    pivot_col <- which.min(min_tableu[nrow(min_tableu),])
    
    # Find pivot row
    # Create a new vector of test ratios
    test_ratios <- min_tableu[,ncol(min_tableu)] / min_tableu[,pivot_col]
    
    # Replace all non-positive ratios with Inf
    for (i in 1:length(test_ratios)) {
      if (test_ratios[i] <= 0) {
        test_ratios[i] <- Inf
      }
    }
    
    pivot_row <- which.min(test_ratios[1:nrow(min_tableu) - 1])
    
    if (is.infinite(min(test_ratios))){
      print("No pivot row found.")
      print("Infeasible.")
      break
    }
    
    
    # Divide pivot row by pivot element
    min_tableu[pivot_row,] <- min_tableu[pivot_row,] / min_tableu[pivot_row,pivot_col]
    
    # Subtract pivot row from all other rows
    for (i in 1:nrow(min_tableu)) {
      if (i != pivot_row) {
        min_tableu[i,] <- min_tableu[i,] - min_tableu[pivot_row,] * min_tableu[i,pivot_col]
      }
    }
    
    iterations <- iterations + 1
    if (iterations == MAX_ITERATIONS) {
      print(min_tableu)
      print("Maximum number of iterations reached.")
      if (min(min_tableu[nrow(min_tableu),]) < 0) {
        print("Infeasible.")
      }
    }
  }
  
  return(min_tableu)
}