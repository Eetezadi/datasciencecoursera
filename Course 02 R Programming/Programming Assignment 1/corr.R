# Write a function that takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and nitrate for
# monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  # Generate a list with full file names
  files_list <- list.files(directory, full.names = TRUE)
  
  # Read defined file names
  data_list <-  lapply(files_list, read.csv)
  
  # Compute correlation for complete cases
  out <- vector() # Empty vector to be filled
  for (i in seq_along(data_list)) {
    
    # Logical vector with complete cases
    valid <- complete.cases(data_list[[i]])
    
    if (sum(valid) < threshold) {
      next
    } else {
      out <- append(out,
                    cor(data_list[[i]][valid, "sulfate"],
                        data_list[[i]][valid, "nitrate"]))
    }
  }

  out
}