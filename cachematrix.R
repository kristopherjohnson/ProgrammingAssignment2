## A pair of functions that cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse.
##
## Returns list of functions "set", "get", "setinv", "getinv".

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(x) inv <<- x
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the mean of the special "vector" created with the above function.
##
## Return a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
