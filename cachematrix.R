## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and generates a list containing the matrix
## and methods to get and store the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
      x <<- y
      inv <<- NULL #if the matrix is changed, the inverse must be wiped out
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This method will return the inverse of the matrix passed to makeCacheMatrix
## It operates by first checking a cache to see if we already have solved the matrix
## If so, it returns that cached value
## If not, it computes the inverse and caches it
cacheSolve <- function(x, ...) {
    #first, get the inverse from the cache
    inv <- x$getinv()
    
    #if non-null, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    #otherwise, compute it and store it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
