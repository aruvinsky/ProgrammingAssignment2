## Programming Assignment 2: Caching the Inverse of a Matrixless 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.


## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # x$set:
    # given a matrix y, this function sets the internal matrix variable of this special matrix to y
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    # x$get:
    # this function returns the current matrix value, x
    get <- function() x
    # x$setInv:
    # given the inverted matrix of x, this function sets the variable inv to that inverted matrix
    setInv <- function(inverse) inv <<- inverse
    # x$getInv:
    # this function returns the current inverted matrix value of x, namely inv
    getInv <- function() inv

    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x$get()

  # Return a matrix that is the inverse of 'x'
  # first, make sure that the inverse has been calculated and is not NA
  invMat <- x$getInv()
  if (!is.null(invMat)) {
    message("getting cached matrix inverse...")
    return(invMat)
  }
  # if this point is reached, it can only be because the inverse has not yet been calculated
  # calculate and set the inverse of x
  invMat <- solve(mat, ...)
  x$setInv(invMat)
  return(invMat)
}
