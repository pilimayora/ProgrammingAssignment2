# This script contains two functions used for caching the inverse of a matrix.
# 
# makeCacheMatrix(): Creates a "special" matrix that can cache its inverse.
# cacheSolve(): Returns the inverse of a "special" matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a "special" matrix that can cache its inverse
  # Args:
  #   x: a matrix. Default is an empty matrix.
  # Returns:
  #   A list with all the accessible attributes/functions of the "special" matrix.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Returns the inverse of a "special" matrix.
  # If the inverse has already been calculated it retrieves the inverse from the cache.
  # Otherwise, it calculates the inverse and it sets it in the cache.
  # Args:
  #   x: a "special" matrix (created with the makeCacheMatrix() function). 
  #   The function assumes the matrix is invertible.
  # Returns:
  #   A matrix that is the inverse of 'x'.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  message("Caching inverse matrix")
  x$setinverse(inv)
  inv
}
