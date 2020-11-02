## The makeCacheMatrix and cacheMatrix combination will calculate the
## inverse of the inputted matrix, cache it's inverse, and retrieve the
## inverse if it exists in the cache.
## The functions levy lexical scoping in order to retrieve variables
## from the parent function that are left unspecified.

## The set function specifies which data points to access.
## Variable x is assigned to the input argument in the parent function.
## Variable m is defaulted to NULL in the parent function in order to
## clear the cache.
## The get function is solving for the matrix inverse.
## This get function is looking to the parent environment to obtain the 
## correct definition of variable m 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The cachesolve function will retrieve data from the cache if it exists
## for the function argument, otherwise, it will calculate the matrix
## inverse

cachesolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

