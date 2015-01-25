## This pair of functions cache the inverse of a matrix to avoid 
## computing it repeatedly. 

## The first function creates a special matrix object that can 
## cache its inverse. This is implemented as a list of functions.
makeCacheMatrix <- function(A = numeric()) {
  inv_A <- NULL
  set <- function(y) {
    A <<- y
    inv_A <<- NULL
  }
  get <- function() A
  setinverse <- function(inverse) inv_A <<- inverse
  getinverse <- function() inv_A
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned 
## by makeCacheMatrix by checking if the inverse is already calculated 
## and returning it from the cache. Otherwise it calculates the inverse 
## and stores it in the cache.

cacheSolve <- function(A, ...) {
  inv_A <- A$getinverse()
  if(!is.null(inv_A)) {
    message("getting cached inverse matrix")
    return(inv_A)
  } else { 
    data <- A$get()
    inv_A <- solve (data, ...)
    A$setinverse(inv_A)
    return(inv_A)}
 
}