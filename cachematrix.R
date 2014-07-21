## 'makeCacheMatrix' and 'cacheSolve' are a pair of functions that allow
## caching the inverse of a matrix to avoid calculating it more than once.
##
## Sample usage:
## mat <- matrix(1:4, 2, 2)
## cachmat <- makeCacheMatrix(mat)
## invmat <- cacheSolve(cachmat)

## 'makeCacheMatrix' returns a list that bundles a matrix and its inverse.
## The matrix can be accessed via '$get()' and changed via '$set()'.
## The inverse can be accessed via '$getInv()' and changed via '$setInv()'.

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse.
  
  # cached inverse
    invmat <- NULL
  
  # access
  get <- function() x
  getInv <- function() invmat
  
  # mutate 
  set <- function(newX) {
    x <<- newX
    invmat <<- NULL
  }
  setInv <- function(newInv) invmat <<- newInv
  
  # return 
  list(get = get, set = set,
       getInv = getInv, setInv = setInv)
}

##This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  invmat <- x$getInv()
  
  # invert if necessary
  if (is.null(invmat)) {
    
    mat <- x$get()
    invmat <- solve(mat, ...)
    
    ##solve function in R For example, if X is a square invertible matrix,
    ##then solve(X) returns its inverse.
    
    x$setInv(invmat)
  }
  
  # return the inverse matrix
  
  invmat
}
