# Introduction
# 
# This second programming assignment will require you to write an R
# function that is able to cache potentially time-consuming computations.
# For example, taking the mean of a numeric vector is typically a fast
# operation. However, for a very long vector, it may take too long to
# compute the mean, especially if it has to be computed repeatedly (e.g.
# in a loop). If the contents of a vector are not changing, it may make
# sense to cache the value of the mean so that when we need it again, it
# can be looked up in the cache rather than recomputed. In this
# Programming Assignment you will take advantage of the scoping rules of
# the R language and how they can be manipulated to preserve state inside
# of an R object.
# 
# ### Example: Caching the Mean of a Vector
# 
# In this example we introduce the `<<-` operator which can be used to
# assign a value to an object in an environment that is different from the
# current environment. Below are two functions that are used to create a
# special object that stores a numeric vector and caches its mean.
# 
# The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean


makeVector <- function(x = numeric()) {
  
  # Initialize m which contains the mean
  m <- NULL
  
  # Function to set value of a vector. "<<-" causes a search to be made through
  # parent environments for an existing definition of the variable being
  # assigned. If such a variable is found (and its binding is not locked) then
  # its value is redefined, otherwise assignment takes place in the global
  # environment.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Function to get value of a vector
  # Since x is not in this function it will look in the parent.frame
  get <- function() x
  
  # Function to set the mean<<-" causes a search to be made through
  # parent environments for an existing definition of the variable being
  # assigned. If such a variable is found (and its binding is not locked) then
  # its value is redefined, otherwise assignment takes place in the global
  # environment.
  setmean <- function(mean) m <<- mean
  
  # Function to get the mean
  # Since m is not in this function it will look in the parent.frame
  getmean <- function() m
  
  # Output when called, appears to be useless
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector"
# created with the above function. However, it first checks to see if the
# mean has already been calculated. If so, it `get`s the mean from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the `setmean`
# function.

cachemean <- function(x, ...) {
  
  # Get the value of mean of the vector
  m <- x$getmean()
  
  # If mean was previously calculated it returns it directly
  # No additional computation
  if (!is.null(m)) {
    message("getting cached data")
    return(m) # This will end all further execution of code
  }
  
  # Get the original data put into makeVector()
  data <- x$get()
  
  # Calculate the mean of data with potential arguments
  m <- mean(data, ...)
  
  # Put calculated mean into makeVector()
  x$setmean(m)
  
  # Output of the mean
  m
}
