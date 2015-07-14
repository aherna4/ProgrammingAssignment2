# The functions, makeCacheMatrix and cacheSolve, compute the inverse of a 
# special "matrix" and cache it so that it does not have to be recomputed.
# The main goal of our functions is to save computing time, in the case when an
# inverse needs to be computed repeatedly. Note: we assume that the matrix
# supplied is always invertible.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by the 
# makeCacheMatrix above. If the inverse has already been calculated and the matrix
# has not changed, then cacheSolve should retrieve the inverse from cache.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Simple Test Case on 1000 X 1000 matrix
# n <- 1000
# a <- matrix(sample(1:n, (n^2), replace = TRUE), n, n)
# b <- makeCacheMatrix(a)
# Comparison of cacheSolve runtimes
# system.time(b1 <- cacheSolve(b))
# system.time(b2 <- cacheSolve(b))
# Comparison of the outputs
# identical(b1, b2)
