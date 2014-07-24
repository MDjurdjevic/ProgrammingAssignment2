## Below is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function (mat) {
    x <<- mat
    m_inv <<- NULL
  }
  get <- function () x
  setInverse <- function(mInv) m_inv <<- mInv
  getInverse <- function() m_inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

  
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached matrix")
    return (m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
