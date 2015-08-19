# Write a function called best that take two arguments: the 2-character 
# abbreviated name of a state and an outcome name. The function reads the 
# outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the best (i.e. lowest) 30-day mortality for the 
# specified outcome in that state. The hospital name is the name provided in the
# Hospital.Name variable. The outcomes can be one of “heart attack”, “heart 
# failure”, or “pneumonia”. Hospitals that do not have data on a particular 
# outcome should be excluded from the set of hospitals when deciding the 
# rankings.
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

best <- function(state, outcome){
  
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid states, valid outcomes and cols
  valid_states <- levels(as.factor(data[,7]))
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_cols = c(11, 17, 23); # from manual in order
  
  # Throw if not valid state
  if (!(state %in% valid_states)) stop("invalid state")
  if (!(outcome %in% valid_outcomes)) stop("invalid outcome")
  
  # Determine column number for outcome
  outcomes_col <- outcome_cols[match(outcome, valid_outcomes)]
  
  # Select only rows with state and no "Not Available"
  rcond <- data[,7] == state & !(data[, outcomes_col] == "Not Available")
  # Extract hospital.name and outcomes column.
  death_rates <- data[rcond, c(2, outcomes_col)]
  
  # Select hospital(s) with minimum death rates 
  sel <- which(as.numeric(death_rates[,2]) == min(as.numeric(death_rates[,2])))
  hospitals <- death_rates[,1][sel]
  
  # Return hospital that comes first in alphabet
  return(sort(hospitals))
}