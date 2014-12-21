## Functions that cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
            x <<- y
            inverted <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverted) inverted <<- new_inverted
    getinverse <- function() inverted
    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix object.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverse()
    if(!is.null(inverted)) {
            message("getting cached data")
            return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverse(inverted)
    inverted
}
