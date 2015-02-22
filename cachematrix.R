# This script contains 2 functions that implement caching of the matrix caching.
# Once computed, the inverse of the matrix is cached to memory and can be
# retrieved, provided that the contents of the matrix have not changed.
# Computing the inverse of a square matrix is done with the built-in solve 
# function. The script assumes that the input matrix is invertible.

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# The object is a list of functions to set and get the value of the matrix; and
# to set and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # we haven't calculated the inverse yet so it's null
    # setting a new matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x # retrieve the matrix
    
    setinverse <- function(solve) inv <<- solve # set the inverse
    getinverse <- function() inv # get the cached inverse
    # available object methods:
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve function computes the inverse of the special "matrix" returned by
# makeCacheMatrix function above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# the cacheSolve retrieves the inverse from the cache with getinverse function
# and skips the computation.
# Otherwise, it computes the inverse of the matrix and sets the value of the 
# inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # if the inverse has been calculated before, retrieve it and exit
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if not, get the data, compute the inverse, cache it and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    
}