# Write a function called rankall that takes two arguments: an outcome name
# (outcome) and a hospital rank- ing (num). The function reads the
# outcome-of-care-measures.csv file and returns a 2-column data frame containing
# the hospital in each state that has the ranking specified in num. For example
# the function call rankall("heart attack", "best") would return a data frame
# containing the names of the hospitals that are the best in their respective
# states for 30-day heart attack death rates. The function should return a value
# for every state (some may be NA). The first column in the data frame is named
# hospital, which contains the hospital name, and the second column is named
# state, which contains the 2-character abbreviation for the state name.
# Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings.
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

rankall <- function(outcome, num = "best") {
  
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid states, valid outcomes and cols
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  outcome_cols = c(11, 17, 23); # from manual in order
  
  # Throw if not valid outcome
  if (!(outcome %in% valid_outcomes)) stop("invalid outcome")
  
  # Determine column number for outcome
  outcomes_col <- outcome_cols[match(outcome, valid_outcomes)]
  
  # Extract hospital.name, outcomes column in a list by state
  # Remove all rows with "Not Available"
  #rcond <- data[, outcomes_col] != "Not Available"
  
  # Make a list for each state containing a data-frame
  states_l <- split(data, data[,7])

  best_hospital <- character(length(states_l))
  for (i in seq_along(states_l)) {
    
    # Remove rows which dont contain data ("Not Available")
    rcond <- states_l[[i]][, outcomes_col] != "Not Available"
    # Extract hospital.name and outcomes column.
    death_rates <- states_l[[i]][rcond, c(2, outcomes_col)]
    
    # Build vector with ranks based on outcome ASC, hospital name ASC
    rank <- order(as.numeric(death_rates[, 2]), death_rates[, 1])
    
    # Rewrite num
    no_hospitals <- length(death_rates[,1])
    if (num == "best") nnum <- 1
    else if (num == "worst") nnum <- as.numeric(no_hospitals)
    else nnum <- num
    
    # Validate num for integer, length
    if (class(nnum) != "numeric") stop("invalid num")
    else if (nnum %% 1 != 0) stop("invalid num")
    else if (nnum > no_hospitals | nnum < 1) best_hospital[i] <- "NA"
    
    
    # Return best hospital that comes first in alphabet
    if (is.na(death_rates[rank[nnum], 1])) best_hospital[i] <- "NA"
    else best_hospital[i] <- death_rates[rank[nnum], 1]  
  }
  
  result <- data.frame(hospital = best_hospital, state = names(states_l))
  return(result)
}