## The following are a set of functions that 1) cache and 2) compute the inverse of a matrix (Programming Assignment 2)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtx = matrix()) {
	inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
above

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
