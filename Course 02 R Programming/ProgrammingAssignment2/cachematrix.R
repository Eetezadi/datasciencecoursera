## 
## This file contaiins 2 functions
##  1) makeCacheMatrix() - a helper function
##  2) cacheSolve() - computes and caches inversion of matrices
##
## Usage:
##  1) Create a special matrix with cached_matrix <- makeCacheMatrix(matrix)
##  2) Compute the inversion with cacheSolve(cached_matrix)
##  3) If you call cacheSolve() a second time, matrix will be retrieved from
##     cache.

## Creates a special matrix vector which can be cached
## Arguments:
##  x: matrix that can be inverted (squared)

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize invm to contain the inverted matrix
  invm <- NULL
  
  # Set value of the matrix. "<<-" causes a search to be made through parent
  # environments for an existing definition of the variable being assigned. If
  # such a variable is found (and its binding is not locked) then its value is
  # redefined, otherwise assignment takes place in the global environment.
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  # Get value of the matrix
  # Since x is not in this function it will look in the parent.frame
  get <- function() x
  
  # Set the value of the matrix. "<<-" causes a search to be made through parent
  # environments for an existing definition of the variable being assigned. If
  # such a variable is found (and its binding is not locked) then its value is
  # redefined, otherwise assignment takes place in the global environment.
  setinv <- function(inv) invm <<- inv
  
  # Get the matrix
  # Since invm is not defined in this function it will look in the parent.frame
  getinv <- function() invm
  
  # Make above functions available as a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inversion of a matrix if this inversion has not been already
## carried out. Otherwise just returns inverted matrix.
## Arguments:
##  x   : Vector created with makeCacheMatrix(),
##  ... : Additional arguments for solve()
## Dependency:
##  makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  # Get a (potentially) previously computed inverted matrix out of
  # makeCacheMatrix()
  invm <- x$getinv()
  
  # If cached matrix exists return it directly and exit
  if (!is.null(invm)) {
    message("Returning inverted matrix from cache:")
    return(invm) # This will end all further execution of code
  }
  
  # Get the original data put into makeCachedMatrix()
  data <- x$get()
  
  # Calculate the inverse of the matrix with potential additional arguments
  invm <- solve(data, ...)
  
  # Put calculated inverted matrix into makeCachedVector()
  x$setinv(invm)
  
  # Output of the inverted matrix
  invm
}
