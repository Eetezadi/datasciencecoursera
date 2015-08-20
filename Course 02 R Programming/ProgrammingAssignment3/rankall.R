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


# Determines hospital for each state according to outcome and num
# Dependencies: dplyr
# Arguments:
#   outcome: factor: heart attack”, “heart failure” or “pneumonia”
#   num: integer, ranking of the hospital, "best" | "worst" also allowed

rankall <- function(outcome, num = "best"){
  
  # Read data into object
  data = read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vectors with valid outcomes and cols
  o_valid <- data.frame(input = c("heart attack", "heart failure", "pneumonia"),
                        colname = c("Heart.Attack", "Heart.Failure", "Pneumonia"))
  
  # Throw if not valid outcome
  if (!(outcome %in% o_valid$input)) stop("invalid outcome")
  
  # Create the right column name based on outcome
  outcome_col <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.",
                       o_valid$colname[which(o_valid$input == outcome)])
  
  # Write filter based on num
  # n() selects the last row in dplyr
  if (num == "best") num <- 1
  else if (num == "worst") num <- "n()"
  num_filter <- paste0("row_number() == ", num)
  
  
  # dplyr pipes action: Select and rename columns, filter invalid values, group
  # by state, convert outcome to number, arrange by outcome and break ties with 
  # hospital name, filter for num and finally select hospital
  mort_df <- data %>% select_(hospital = "Hospital.Name", 
                              outcome = outcome_col,
                              state = "State") %>%
                      filter(outcome != "Not Available") %>%
                      group_by(state) %>%
                      mutate(outcome = as.numeric(outcome)) %>%
                      arrange(outcome, hospital) %>%
                      filter_(num_filter) %>% select(hospital)
  
  # The filter in the statement above will drop empty groups. Currently there is
  # no way to prevent that (https://github.com/hadley/dplyr/issues/341).
  # Therefore all a sorted list of all states will be extracted again
  all_states_df <- data %>% select(state = State) %>% 
                            arrange(state) %>%
                            distinct(state)

  # Joins mortalities and states together. If there are no values for a state
  # "NA" will be introduced
  ranked_hospitals <- right_join(mort_df, all_states_df)
  
  return(ranked_hospitals)
}
