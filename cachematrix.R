
## makeCacheMatrix is used for allocating space for the cached inverse matrix
## and defining functions for setting the matrix and its inverse
## and retrieving the matrix and its inverse (when the latter has been set).
## It takes the matrix as argument and returns a list of 4 functions for setting
## and getting the matrix and for setting and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- matrix(nrow=nrow(x), ncol=ncol(x))
     set <-function (y) {
          x <<- y
          inv <- matrix(nrow=nrow(x), ncol=ncol(x))
     }
     get <- function() x
     setinverse <- function(inverted) inv <<- inverted
     getinverse <- function() inv
     
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     
}

## cacheSolve is a function for retrieving the inverse of the matrix defined as
## argument to cacheMatrix by either solving it (first time) or returning the
## cached inverse. It takes the list returned by makeCacheMatrix as argument.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if (!is.na(inv[1,1])) {
          message ("getting cached inverse")
          return(inv)
     }
     mymatrix <- x$get()
     inv <- solve(mymatrix)
     x$setinverse(inv)
     
}
