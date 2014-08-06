## The pair of functions below cache a matrix, and, as part of the first access 
## to the inverse of the matrix, cache the inverse as well
## This is done using R closures.

## makeCacheMatrix takes a matrix, caches the matrix using R closures,
## and defines functions to get/set the data & get/set the inverse
## The set* functions cache the supplied values using R closures

makeCacheMatrix <- function(x = matrix()) {
  # set the cached inverse to null (could set to real inverse of x at this time)
  inv <- NULL
  
  # Function to set saved data to new value. Resets cached inverse to NULL at same time
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return base matrix
  get <- function() x
  
  #Save specified value as new cached inverse
  setinv <- function(i) inv <<- i
  
  #Return currently cached inverse value
  getinv <- function() inv
  
  # return the defined functions as a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Helper function to access the caching functionality supplied by makeCacheMatrix
## Checks if the inverse has been cached. If it has, it is returned from cache.
## If not, calculates inverse, saves it to cache, then returns value. 
## Input is an object returned from a previous call to makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # get the cached inverse value
  inv <- x$getinv()
  
  # if not null, return the cached value & exit
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  
  # Otherwise get the cached matrix, calculate the inverse using solve, 
  # saves the value to cache, then returns the value
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setinv(inv)
  inv
  
}
