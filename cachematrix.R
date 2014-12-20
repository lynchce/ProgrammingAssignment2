## This collection of functions is designed to efficiently
## invert a matrix via a cacheing strategy.  When a matrix
## is first passed to cacheSolve, it checks to see if the
## inversion has already been computed.  If the computation
## exists, the old result is retreived.  It should be noted
## that the matrix being passed must be invertible.

## makeCacheMatrix establishes a free variable
## which is used to store the inverse of the matrix passed in

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # inv will contain the computed inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  invisible(list(set = set, get = get, 
                 setinv = setinv, getinv = getinv))
}

## cacheSolve accepts the matrix that we want to invert. If
## this matrix has been inverted previously, it returns the
## cached version.  If it was not previously inverted, it 
## computes the inverse and caches the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()     #Retrieve the matrix
  inv <- solve(mat)  #Compute the inverse
  x$setinv(inv)      #Store the inverse
  inv
}