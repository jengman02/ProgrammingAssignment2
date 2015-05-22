## This is a set of functions that are able to cache the potentially
## time-consuming computations of the inverse of matrices.
##
## I solved this assignment with the help of DanieleP's tutorial linked to 
## from the R Programming community site, and which is found at:
## https://github.com/DanieleP/PA2-clarifying_instructions
## 
## For the desricptions of the functions I adapted Dr Peng's descriptions of
## the example functions.

## This function returns a list of functions that can create a matrix object
## and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse_global) inverse <<- inverse_global
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function takes the matrix object from the function makeCacheMatrix and
## computes its inverse. However, if the same matrix object is used again,
## after its inverse has already been computed, then this function retrieves
## the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
