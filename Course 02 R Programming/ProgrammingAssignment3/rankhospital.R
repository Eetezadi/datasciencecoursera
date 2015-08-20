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


# Determines the hospital for each outcome and state at a specific ranking
# Dependencies: dplyr
# Arguments:
#   state: character: 2 Letter code for the state
#   outcome: factor: heart attack”, “heart failure” or “pneumonia”
#   num: integer, ranking of the hospital, "best" | "worst" also allowed

rankhospital <- function(state, outcome, num = "best"){
  
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid states, valid outcomes and cols
  o_valid <- data.frame(input = c("heart attack", "heart failure", "pneumonia"),
                        colno = c(11, 17, 23),
                        colname = c("Heart.Attack", "Heart.Failure", "Pneumonia"))
  s_valid <- data %>% distinct(State) %>% select(State)
  
  # Throw if not valid state or valid outcome
  if (!(state %in% s_valid$State)) stop("invalid state")
  if (!(outcome %in% o_valid$input)) stop("invalid outcome")
  
  
  # Creating the right column name based on outcome
  outcome_col = paste0("Hospital.30.Day.Death..Mortality..Rates.from.",
                       o_valid$colname[which(o_valid$input == outcome)])
  
  # dplyr pipes action: Select and rename columns, filter for state and invalid 
  # values, convert to number and finally arrange by outcome and break ties with
  # hospital name
  mort_df <- data %>% select_(hospital = "Hospital.Name", 
                              outcome = outcome_col,
                              states = "State") %>% 
                      filter(states == state & outcome != "Not Available") %>%
                      mutate(outcome = as.numeric(outcome)) %>%
                      arrange(outcome, hospital)

  
  # Rewrite num
  if (num == "best") num <- 1
  else if (num == "worst") num <- length(mort_df$hospital)
  

  # Return hospital according to ranking if ranking exists
  if (is.na(mort_df$hospital[num])) return("NA")
  else return(mort_df$hospital[num])
}