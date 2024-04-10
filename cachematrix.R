makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(matrix) {
    x <<- matrix
    cache <<- NULL
  }
}

get <- function() x

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}