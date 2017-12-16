## Functions that cache the inverse of a matrix

## Creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Solve the inverse of matrix returned by "makecachematrix" above. The value 
## will be retrieved if set otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  info<- x$get()
  inv<- solve(info)
  x$setInverse(inv)
  inv
}
