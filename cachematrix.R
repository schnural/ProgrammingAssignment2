## As computing the inverse of a matrix may be quite costly it makes sense
## to cache the inverse of a matrix to retrieve the result the next time without
## computing the inverse again. The two functions makeCacheMatrix and cacheSolve are meant
## to fullfill this task.

## makeCacheMatrix is a function that creates a matrix object which is 
## able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(minverse) m <<- minverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of a square matrix.
## The matrix is of special kind as created by makeCacheMatrix. If the inverse 
## is already created and the matrix has not changed, then cacheSolve returns 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.
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
