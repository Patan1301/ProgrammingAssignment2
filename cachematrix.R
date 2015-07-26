# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached inverse value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL
  
  # store a matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() x
  
  # cache the given argument 
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # get the cached value
  getInverse <- function() {
    inv}
  
  # return a list. Each named element of the list is a function
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix. It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the inv via setinverse function.

#Assuming the matrix given is always invertible

cacheSolve <- function(x, ...) {
  # get the cached value
  inv <- x$getInverse()
  
  # if a cached value exists return it
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)

  # return the inverse
  inv
}
