## These functions implemented a method to cache the inverse of 
##    any given matrix

## The first function create a "CacheMatrix" object that can cache
## the inverse of a given matrix
## The function returns a list with 4 functions that can each:
##  1. initialize the CacheMatrix by a given matrix parameter;
##  2. retrieve the matrix stored in the CacheMatrix;
##  3. set the Cache Inverse by a given matrix parameter;
##  4. retrieve the CacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(s) m <<- s
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The second function returns the inverse from the given CacheMatrix.
## If the Cache Inverse is already calculated, the function will simply
##    retrieve the inverse from the cache;
## Otherwise, it will calculate the inverse, store the inverse in the 
##    cache, and eventually return the calculated inverse.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("retrieved solve")
    return(m)
  }
    
  data <- x$get()
  m <- solve(data,...)
  x$setSolve(m)
  message("set solve")
  m
        ## Return a matrix that is the inverse of 'x'
}
