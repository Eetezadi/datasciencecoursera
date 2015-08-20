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
# Dependencies: dplyr
# Arguments:
#   state: character: 2 Letter code for the state
#   outcome: factor: heart attack”, “heart failure” or “pneumonia”

best <- function(state, outcome){
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid states, valid outcomes and cols
  o_valid <- data.frame(input = c("heart attack", "heart failure", "pneumonia"),
                        colname = c("Heart.Attack", "Heart.Failure", "Pneumonia"))
  s_valid <- data %>% select(state = State) %>% 
                      arrange(state) %>%
                      distinct(state)

  # Throw if not valid state or valid outcome
  if (!(state %in% s_valid$state)) stop("invalid state")
  if (!(outcome %in% o_valid$input)) stop("invalid outcome")
  
  # Creating the right column name based on outcome
  outcome_col = paste0("Hospital.30.Day.Death..Mortality..Rates.from.",
                       o_valid$colname[which(o_valid$input == outcome)])
  
  # dplyr pipes action: Select and rename columns, filter for state and invalid 
  # values, convert to number, arrange by outcome and break ties with hospital
  # name and finally select top value
  mort_df <- data %>% select_(hospital = "Hospital.Name", 
                             outcome = outcome_col,
                             states = "State") %>% 
                      filter(states == state & outcome != "Not Available") %>%
                      mutate(outcome = as.numeric(outcome)) %>%
                      arrange(outcome, hospital) %>%
                      slice(1)
  
  # First element is best hospital
  return(mort_df$hospital)
}