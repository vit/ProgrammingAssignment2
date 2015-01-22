## This functions help to avoid repeated computations
## of the inverse matrix for the same original matrix

## Creates a "matrix" object that can store the original matrix
## and cache its (calculated somewhere outside) inverse.
## The object has four methods (setters and getters for
## the original matrix data and the inverse data).

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_matrix) im <<- inv_matrix
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## For given "matrix" object returns its inverse.
## If the inverse matrix is not cached yet, calculates it and remembers
## (caches) the result.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
