## Usage: wrap your matrix in makeCacheMatrix and call cacheSolve with the
## wrapped matrix to get the inversed matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        getinverse <- function() m
        setinverse <- function(inv) m <<- inv
        list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("Read from cache")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
