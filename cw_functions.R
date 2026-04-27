# Q1
# Just defining a function to check if the corrected code is giving result or not
# Simulates a single game of Mountain Climb, calls play_mc() within it, and returns the turns required to finish the game 
simulate_game <- function() {
  # Initializing the starting position at the base of the mountain (position 0)
  pos <- 0
  
  # Initializing a counter to count the number of turns taken in the game
  turns <- 0
  
  # The game is continued as long as the position is below 70 and turns are taken
  while (pos < 70) {
    # Performs the Markov chain simulation for one turn and its consequence (transition)
    pos <- play_mc(pos) 
    
    # Increasing the counter for turns with each transition
    turns <- turns + 1
  }
  
  return(turns) # Returns the total number of turns taken to reach the summit (position 70)
}



# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------



# Q2
# Function to simulate a full Mountain Climb game from start to finish
play_mc_full <- function(
    start_pos = 0,                     # Starting position (default = 0)
    max_turns = 500,                   # Safety limit so the loop does not run forever
    slippery_squares = seq(9, 63, 9),  # Multiples of 9 denoting slippery squares
    climbing_squares = seq(16, 64, 16),# Multiples of 16 denoting climbing squares
    slip_back_range = 1:3,             # On slippery squares, player slides back by 1, 2 or 3 positions
    climb_up_amount = 5,               # On climbing squares, player climbs +5 positions
    max_pos = 70                       # Summit position (game ends here)
) {
  
  # Setting the player's current position
  pos <- start_pos   
  
  # Records the starting position
  pos_history <- c(pos)              
  
  # Loop for each turn up to max_turns
  for (turn in 1:max_turns) {
    
    # Rolls a fair six-sided die; values ∈ {1, 2, 3, 4, 5, 6}
    # Returns an integer from 1 to 6
    die <- sample.int(6, 1)   
    
    # Moves forward by the value on the rolled die
    pos <- pos + die                 
    
    # ---- Applying SLIPPERY SQUARE rule ----
    # If the new position is one of the slippery squares
    if (pos %in% slippery_squares) {
      # Slides back by a random position from slip_back_range (1, 2, or 3)
      pos <- pos - sample(slip_back_range, 1)
      # Makes sure the player does not slide below position 0 
      # by returning the maximum value between 0 and pos value (always > 0)
      pos <- max(0, pos)
    }
    
    # ---- Applying CLIMBING SQUARE rule ----
    # If the new position is one of the climbing squares
    if (pos %in% climbing_squares) {
      # Moves the player up by the climb-up bonus (pos + 5)
      pos <- pos + climb_up_amount
    }
    
    # ---- Checking if the SUMMIT is reached ----
    if (pos >= max_pos) { # if pos is greater than position 70 (max_pos)
      pos <- max_pos      # Capping or limiting the position at the summit (cannot exceed 70)
      
      # Adding final position to history
      pos_history <- c(pos_history, pos)
      
      # Ending the game and returning results of 2 values: pos_history and turns_taken
      return(list(
        pos_history = pos_history,   # A vector containing all positions visited
        turns_taken = turn           # Number of turns needed to reach summit
      ))
    }
    
    # Storing the position at the end of this turn
    pos_history <- c(pos_history, pos)
  }
  
  # If max_turns passed and summit not reached (failsafe)
  return(list(
    pos_history = pos_history,       # Full movement history
    turns_taken = max_turns          # Reached the limit, not the summit
  ))
}

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------





# Q3

# Function to simulate multiple Mountain Climb games and summarise the results
simulate_climbs <- function(
    
  n_simulations = 100,                # Number of independent games to simulate
  start_pos = 0,                      # Starting position of the player in each game
  max_turns = 500,                    # Maximum number of turns allowed per game
  slippery_squares = seq(9, 63, 9),   # Slippery squares (multiples of 9)
  climbing_squares = seq(16, 64, 16), # Climbing squares (multiples of 16)
  slip_back_range = 1:3,              # Number of steps to slide back on slippery squares
  climb_up_amount = 5,                # Number of steps to climb up on climbing squares
  max_pos = 70                        # Summit position (where game ends)
) {
  
  # Creating a numeric vector to store the number of turns taken in each simulation
  turns_vector <- numeric(n_simulations)
  
  # Creating a list to store the full position history for each simulation
  all_histories <- vector("list", n_simulations)
  
  # Looping over the number of simulations
  for (i in 1:n_simulations) {
    
    # Running one complete Mountain Climb game
    # This returns a list containing:
    # (1) pos_history: positions visited
    # (2) turns_taken: total number of turns
    game <- play_mc_full(
      start_pos,                  # Starting position
      max_turns,                  # Maximum number of turns
      slippery_squares,           # Slippery squares
      climbing_squares,           # Climbing squares
      slip_back_range,            # Slip-back range
      climb_up_amount,            # Climb-up amount
      max_pos                     # Summit position
    )
    
    # Storing the number of turns taken for this simulation
    turns_vector[i] <- game$turns_taken
    
    # Storing the full position history for this simulation
    all_histories[[i]] <- game$pos_history
  }
  
  # Calculating summary statistics for the number of turns taken
  turns_stats <- c(
    mean   = mean(turns_vector),    # Average number of turns
    median = median(turns_vector),  # Median of the distribution
    sd     = sd(turns_vector)       # Standard deviation 
  )
  
  # Returning all results as a single list
  return(list(
    turns_vector = turns_vector,    # Turns taken in each simulation
    turns_stats = turns_stats,      # Mean, median, and SD of turns
    all_histories = all_histories   # Position histories of all simulations
  ))
}


# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------





# Q4 
# Modified simulate_climbs() function called simulate_pos_at function
# Function to simulate the player's position after a fixed number of turns
simulate_pos_at <- function(
    
  n_simulations = 100,            # Number of independent games to simulate
  turns = 10,                     # Fixed number of turns to simulate in each game
  start_pos = 0,                  # Starting position of the player
  slippery_squares = seq(9, 63, 9),   # Slippery squares (multiples of 9)
  climbing_squares = seq(16, 64, 16), # Climbing squares (multiples of 16)
  slip_back_range = 1:3,          # Number of steps to slide back on slippery squares
  climb_up_amount = 5,            # Number of steps to climb up on climbing squares
  max_pos = 70                    # Maximum position (summit)
) {
  
  # Creating a numeric vector to store the final position from each simulation
  final_positions <- numeric(n_simulations)
  
  # Loop over the number of simulations
  for (i in 1:n_simulations) {
    
    pos <- start_pos    # Reset the player's position at the start of each simulation
    
    # Simulates exactly 'turns' number of moves
    for (t in 1:turns) {
      
      die <- sample.int(6, 1)     # Roll a fair six-sided die (1–6)
      pos <- pos + die            # Move the player forward by the die roll
      
      # If the player lands on a slippery square
      if (pos %in% slippery_squares) {
        # Slide back by a random amount from slip_back_range
        pos <- max(0, pos - sample(slip_back_range, 1))
      }
      
      # If the player lands on a climbing square
      if (pos %in% climbing_squares) {
        # Move forward by the climbing bonus
        pos <- pos + climb_up_amount
      }
      
      # Ensure the player does not move beyond the summit
      pos <- min(pos, max_pos)
    }
    
    # Store the final position reached after the fixed number of turns
    final_positions[i] <- pos
  }
  
  # Return the final positions and their summary statistics
  return(list(
    positions = final_positions,  # Final position after 'turns' in each simulation
    mean = mean(final_positions), # Average position reached after 'turns' turns
    variance = var(final_positions) # Variance of positions (spread of outcomes)
  ))
}

# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------


# Q6
# Function update_bmi() for recalculating BMI, removing implausible values, and 
# removing multivariate outliers using PCA
update_bmi <- function(data) {
  
  bmi_data_copy <- as.data.frame(data)
  
  # Recalculating BMI and assigning the corrected value under a new column ("bmi_recalc") in the existing dataset
  # Weight in kilograms and height in centimetres
  # Height is first converted from centimetres to metres before squaring to ensure consistency with the standard BMI formula
  bmi_data_copy$bmi_recalc <- 
    bmi_data_copy$weight / ((bmi_data_copy$height / 100)^2)
  
  # Replacing the original BMI with the recalculated values
  bmi_data_copy[, 4] <- bmi_data_copy[, 10]
  # so now bmi_recalc (column 10) values are retained in bmi (column 4)
  
  # Removing implausible BMI values and excluding unwanted columns for downstream PCA
  bmi_clean <- bmi_data_copy[, -c(1, 10)]    # Excludes columns 1 and 10
  # Column 1 = idx (meaningless identifier)
  # Column 10 = bmi_recalc (Recalculated BMI values); redundant
  
  # Removing non-numeric sex column which is column 8 in the new dataset
  # Done separately for downstream analysis
  bmi_clean_new <- bmi_clean[, -c(8)]
  
  # Retaining only complete observations for PCA
  numeric_data <- bmi_clean_new[complete.cases(bmi_clean_new), ]
  
  # Performing PCA
  pca_res <- prcomp(numeric_data, 
                    center = TRUE,  # Centers each variable by subtracting its mean
                    scale. = TRUE)  # Scales variables by unit variance
  
  # Extracting scores for the first two principal components
  scores <- pca_res$x[, 1:2]
  
  # Computing distances of samples from the PCA origin (centre)
  # Scores are scaled before computing squared Euclidean distance to ensure comparability
  pca_dist <- sqrt(rowSums(scale(scores)^2))
  
  # Defining outliers using the 97.5% quantile or 97.5th percentile of distances threshold 
  cutoff <- quantile(pca_dist, 0.975)
  
  # Flagging those points exceeding that threshold as outliers
  flag_outlier <- pca_dist > cutoff
  
  # Mapping outlier flags back to original row indices
  pca_rows <- which(complete.cases(bmi_clean))
  
  # Identifying PCA-based outlier rows in the original dataset
  outlier_indices <- pca_rows[flag_outlier]
  
  # Removing PCA-based outliers from the dataset
  bmi_clean_final <- bmi_clean[-outlier_indices, ]
  
  # Returning a cleaned dataset
  data <- bmi_clean_final
  return(data)
}


# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------








