# Coursera programming assignment #2
# R Programming
# 20 AUG 2015
# This program takes an invertible matrix and creates a cached inverse of the input.  The input can be
# retrieved by calling the second function.  Subsequent calls will retrieve the inversed matrix from the cache
# rather than inversing the orginal input.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x <<- y
    f <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) f <<- solve
  getinverse <- function() f
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  f <- x$getinverse()
  if(!is.null(f)) {
    message("Getting the inversed matrix from the cache")
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setinverse(f)
  f
}
