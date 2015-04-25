## These functions allow for the computation of the matrix 
## multiplicative inverse in such a way that once it has 
## been calculated that the results are cached.  Thus, after
## the first request to calculate the inverse, the cached value is simply retrieved from memory.

## returns a list of functions that allow for creation
## and retrieval of a matrix, using <<- to assign values 
## outside the scope of the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix (without checking that it is
## invertible). If the inverse has been calculated already
## it retrieves the cached value, otherwise the inverse is 
## calculated using solve.

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
