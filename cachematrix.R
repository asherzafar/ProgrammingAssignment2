## These functions create a list of functions for a given matrix that 
## compute its inverse, and then caches the solution (if there is one) 
## instead of recalculating it.

## This function creates a list of four elements which are functions to
## get, set, get the inverse, and set the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function takes the list from the above function, and returns
## the inverse of the matrix that was passed to it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
