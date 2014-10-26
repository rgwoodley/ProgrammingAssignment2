######################################################################################################## 
## Functions are use to process and optomize matix operations
## makeCacheMatrix - Creates a specialized object for handling cached matrix
## cacheSolve - Utilized the cached matrix object to retuen the solved matrix
## Author: Rohan Woodley


######################################################################################################## 
## Function: makeCacheMatrix
## Synposis: Create a cached matrix object
## Args:
##  x: A matrix
########################################################################################################
makeCacheMatrix <- function(x = matrix()) {
  solvedCache <- NULL
  set <- function (data) {
    x = data
    solvedCache <<- NULL
  }
  get <- function() x
  setSolved <- function(solved) solvedCache <<- solved
  getSolved <- function() solvedCache
  list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
}

########################################################################################################
## Function: cacheSolve
## Synposis: Returns a cached version of the results (inverse matrix) or solves the matix using 'solve'
##           and then setting the cached results.
## Args:
##  x: Matrix created by 'makeCacheMatrix'
########################################################################################################
cacheSolve <- function(x, ...) {
  solved = x$getSolved()
  if (!is.null(solved)) {
    message("getting cached data")
    return(solved)
  }
  data <- x$get()
  solved <- solve(data, ...)
  x$setSolved(solved)
  solved
}
