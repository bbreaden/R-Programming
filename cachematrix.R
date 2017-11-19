## This script contains two functions. These create a 
## special "matrix" object that can cache its inverse, 
## and then compute the inverse of the special "matrix".
## If the inverse has already been calculated then the 
## inverse is retreived from the cache.

## This makeCachMatrix function creates the "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This cacheSolve function returns a matrix that is the
## inverse of that passed to it (ie. the "x" matrix).
## It begins however by checking whether there is already
## a matrix stored in the cache, in which case it does not
## calculate but simply returns the stored version ("inv").

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Data already exists in cache:...")
                return(inv)
        }
        matx <- x$get()
        inv <- solve(matx, ...)
        x$setInverse(inv)
        inv
}
