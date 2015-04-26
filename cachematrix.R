# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  
  get <- function() m
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible . 
# http://mathworld.wolfram.com/InvertibleMatrixTheorem.html  
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

#  demo  test 
#> mdemo = rbind(c(1, -1/4), c(-1/4, 1))
#> class(mdemo)
#> m = makeCacheMatrix(mdemo)
#> m$get()
#  -- No cache in the first run
# >system.time(print(" First No Cache run  ", cacheSolve(m)))
#  -- Cache in the second run print message  getting cached data
#> system.time(print(" Second Cache   run  ", cacheSolve(m)))
