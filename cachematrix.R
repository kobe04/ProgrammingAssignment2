## These functions take a matrix, cache it and its inverse, and can compute the inverse of the 
## matrix or retrieve the computation from the cache.

## This function creates a cache for a matrix and for its inverse matrix.
## It takes an argument x, which is the matrix to be cached. By default: an empty matrix.
## It then sets the object inv to NULL. This object is later used to store the cache of the
## inversed matrix.
## It returns a list of functions to access the cache.
##  Set allows you to set a new matrix
##  Get allows you to retrieve a matrix from the cache
##  Setinv allows you to set the inverse of the matrix to be cached
##  Getinv allows you to get the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x   <<- y
        inv <<- NULL
    }
    get    <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the cached matrix x.
## it takes the argument x, which is the cached matrix, and the argument ...
## First, it gets the cache of the inverse matrix.
##  If there is a cache, then it will retrieve it and return it.
## If the cache is empty, the function will create the inverse matrix, store it in the cache
## and print it to the console

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if  (!is.null(inv)) {
        message("Getting cached Matrix")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinv(inv)
    inv
}