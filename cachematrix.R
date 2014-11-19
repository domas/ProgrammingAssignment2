## File contains code for two functions for inverting a matrix and saving
## its value so in future calls, function instead of computing the value again
## will just return cached value.

## Define special object that will wrap standard R matrix into a object,
## which will be able to store its inverse matrix.

makeCacheMatrix <- function(value = matrix()) {
    inv <- NULL

    ## Setter for seting the new value for our matrix
    set <- function(newValue) {
        value <<- newValue
        inv <<- NULL
    }
    
    ## Getter for getting the current value (matrix)
    get <- function() {
        value
    }
    
    ## Setter for matrix inversion 
    setInvert <- function(invVal) {
        inv <<- invVal
    } 
    
    ## Getter for matrix inversion
    getInvert <- function() {
        inv
    }
    
    ## As a representation of our object
    ## we return object's functions 
    list(set = set, 
        get = get,
        setInvert = setInvert,
        getInvert = getInvert)
}

## Our version of 'solve' function, that instead of computing inverse matrix 
## each time, caches it's value, so the cached value can be used in the
## future calls. Given 'x' attribute can not be a standard R matrix object but
## a matrix object created using 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
    ## Get value of cache
    inv <- x$getInvert()
    ## if no value returned, we compute new inverse matrix
    if (is.null(inv)) {
        message("calculating new inverse matrix")
        inv <- solve(x$get(), ...)
        x$setInvert(inv)
    }
    ## Return a matrix that is the inverse of 'x'
    inv
}
