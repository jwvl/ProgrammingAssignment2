## These functions create a special 'cache matrix' object.
## By storing the inverse after calculating it once,
## a cache matrix prevents the costly solve() function
## from being called more often than is necessary.

## Returns a 'cache matrix' with getters and setters for the data and inverse.
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cache <<- inverse
    getInverse <- function() cache
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of a cached matrix by getting the cached inverse, or
## calculating and setting it.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
