##
## functions makeCacheMatrix use the solve function to invert a Matrix
#
## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  ##
  ## This function creates a special "matrix" object that can cache its inverse
  ##
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- solve	
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)	
}
cacheSolve <- function(x, ...) {
  ##
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
  ## If the inverse is already calculated and the matrix has not changed, then the cacheSolve
  ## should retrieve the inverse from the cache.
  ##
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data ")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
