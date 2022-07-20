## The goal of the second assignment is to write an R function that is able to cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation. This pair of functions caches the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){ # get inverse from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get() # calculates the inverse
  inv = solve(mat.data, ...)
  x$setinv(inv)  # sets the value of the inverse in the cache
  inv
}