## The two functions should be able to cache computations that
## would otherwise be very time-consuming.

## This first function creates a matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
   invert <- NULL
   setval <- function(y) {
      x <<- y
      invert <<- NULL
   }
   get <- function() {x}
   setInverse <- function(inverse) {invert <<- inverse}
   getInverse <- function() {invert}
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This second function computes the inverse of the matrix 
## that is returned by the first function.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   invert <- x$getInverse()
   if(!is.null(invert)) {
      message("getting cached data")
      return(invert)
   }
   mat <- x$get()
   invert <- solve(mat, ...)
   x$setInverse(invert)
   invert
}
