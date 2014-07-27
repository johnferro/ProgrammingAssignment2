## This file contains funcitons which allow for the caching of the inverse of
## a matrix.

## A function that creates a special "matrix", which is a list containing
## functions to set the value of the matrix, get the value of the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A function that calculates the inverse of a special "matrix of the type
## created with the above function.  First checks to see if the inverse has already
## been calculated and if so, returns a cached version of the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
