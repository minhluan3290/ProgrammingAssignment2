## These two functions below are used to cache the inverse of
## a matrix. These functions help facilitate the calculation of
## matrix inversion, which tends to be costly computation.

## makeCacheMatrix creates a list containing a function to
## get and set the value of the matrix
## get and set the value of inverser of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve functions first check  if the inverse is already 
## calculated. If yes, the functions will return the value of the 
## inverse. If not, it will calculates the value and set the value
## in the cache.
## Assumption: the matris is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
