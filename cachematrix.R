

## Creates a matrix with a caching mechanism for its inverse.
##
## Args: 
##   x: an invertible matrix. WARNING: only invertible matrices should be created. 
## 
## Returns: 
##   A matrix with methods to obtain the cached inverse and to set it, and to get the original matrix and set it
##
## Error handling:
##   None
makeCacheMatrix <- function(x = matrix()) {
  inverted.matrix <- NULL
  Set <- function(y) {
    x <<- y
    inverted.matrix <<- NULL
  }
  Get <- function() {
    return(x)
  }
  SetInvertedMatrix <- function(inverted.matrix.input) {
    inverted.matrix <<- inverted.matrix.input
  }
  GetInvertedMatrix <- function() {
    return(inverted.matrix)
  }
  list(Set = Set, Get = Get,
       SetInvertedMatrix = SetInvertedMatrix,
       GetInvertedMatrix = GetInvertedMatrix)
}


## Sets the inverse matrix for a cacheable matrix built with 'makeCacheMatrix' function
##
## Args:
##   x: the cacheable matrix
## 
## Returns:
##   The inverted matrix
##
## Error handling:
##   None
cacheSolve <- function(x, ...) {
  inverted.matrix <- x$GetInvertedMatrix()
  if(!is.null(inverted.matrix)) {
    message("Getting cached data")
    return(inverted.matrix)
  } else {
    message("Computing and caching matrix")
    original.matrix <- x$Get()
    inverted.matrix <- solve(original.matrix)
    x$SetInvertedMatrix(inverted.matrix)
    return(inverted.matrix)
  }
}
