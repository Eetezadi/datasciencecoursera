# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases.

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Generate a list with full file names
  files_list <- list.files(directory, full.names = TRUE)
  
  # Read defined file names
  data_list <- lapply(files_list[id], read.csv)
  
  # Compute complete cases
  num_complete <- function(x){
    sum(complete.cases(x))
  }
  
  complete_vector <- vapply(data_list, num_complete, 1)
  result_frame = data.frame(id = id, nobs = complete_vector)
  result_frame
}