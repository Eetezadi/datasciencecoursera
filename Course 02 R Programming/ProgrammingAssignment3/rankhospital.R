# Write a function called rankhospital that takes three arguments: the
# 2-character abbreviated name of a state (state), an outcome (outcome), and the
# ranking of a hospital in that state for that outcome (num). The function reads
# the outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the ranking specified by the num argument. For
# example, the call rankhospital("MD", "heart failure", 5) would return a
# character vector containing the name of the hospital with the 5th lowest
# 30-day death rate for heart failure. The num argument can take values “best”,
# “worst”, or an integer indicating the ranking (smaller numbers are better). If
# the number given by num is larger than the number of hospitals in that state,
# then the function should return NA. Hospitals that do not have data on a
# particular outcome should be excluded from the set of hospitals when deciding
# the rankings.
# Handling Ties
# If there is a tie for the best hospital for a given
# outcome, then # the hospital names should be sorted in alphabetical order and
# the first # hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
# and “f” are # tied for best, then hospital “b” should be returned).
# Validity
# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop
# function with the exact message “invalid state”. If an invalid outcome value
# is passed to best, the function should throw an error via the stop function
# with the exact message “invalid outcome”.


# Determines the best state for each outcome
# Arguments:
#   state: character: 2 Letter code for the state
#   outcome: factor: heart attack”, “heart failure” or “pneumonia”

rankhospital <- function(state, outcome, num = "best"){
  
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid states, valid outcomes and cols
  valid_states <- levels(as.factor(data[,7]))
  valid_outcomes <- c("heart attack", "heart failure","pneumonia")
  outcome_cols = c(11, 17, 23); # from manual in order
  
  # Throw if not valid state
  if (!(state %in% valid_states)) stop("invalid state")
  if (!(outcome %in% valid_outcomes)) stop("invalid outcome")
  
  # Determine column number for outcome
  outcomes_col <- outcome_cols[match(outcome, valid_outcomes)]
  
  # Select only rows with state and no "Not Available"
  rcond = data[,7] == state & !(data[, outcomes_col] == "Not Available")
  # Extract hospital.name and outcomes column.
  death_rates <- data[rcond, c(2, outcomes_col)]
  
  # Order the selected hospitals by rank
  ranked_hospitals <- death_rates[with(death_rates, 
                                       order(as.numeric(death_rates[,2]),
                                             death_rates[,1])),] 
  
  # Rewrite num
  no_hospitals <- length(ranked_hospitals[,1])
  if (num == "best") num <- 1
  else if (num == "worst") num <- as.numeric(no_hospitals)
  
  # Validate num for integer, length
  if (class(num) != "numeric") stop("invalid num")
  else if (num %% 1 != 0) stop("invalid num")
  else if (num > no_hospitals | num < 1) return("NA")

  
  # Return best hospital that comes first in alphabet
  return(ranked_hospitals[num,1])
}