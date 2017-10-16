## Function to store inverse matrix computations, and return cached results when precomputed

## Given an input matrix, return a list containing getters and setters for the matrix and inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set new matrix
  set <- function(y) {
    x <<- y
    # clear inverse
    inverse <<- NULL
  }
  # return matrix
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given the cache matrix, return the data if cached and solve and return otherwise
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
