## Caching the inverse of a matrix

# makeCacheMatrix creates a matrix object tha cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  list(get = get, set = set, getinv = getinv, setinv = setinv)
  
}


# cacheSolve computes the inverse of a matrix using the solve function.
# This function also checks and returns the inverse of the matrix if it was already calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getinv()
      
      if(!is.null(inv)) {
          message("Getting cached inverse")
          return(inv)
      }
      
      m <- x$get()
      inv <- solve(m, ...)
      
      x$setinv(inv)
      
      return(inv)
}
