## These two functions allow you to determine the inverse of a matrix and cache 
## the results of the matrix inverse calculation for later use.

## Defines four functions: get, set, getinv and setinv.  These functions can be 
## called by the using any matrix that needs to be inverted.  The program assumes
## the input is an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function caches the result of a matrix inverse and then uses the cached
## result so the inverse does not need to be recalculated every time.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

myInv <- makeCacheMatrix(matrix(c(3,3,3,1,4,2,5:7),nrow=3))

  