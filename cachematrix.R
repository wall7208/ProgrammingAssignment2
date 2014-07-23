## These are utility functions that asssist with the computationally expensive
## matrix inverstion operation by caching previous inverse operations

## Creates a cache matrix object given a matrix as an argument

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns the inverse of a cache matrix object. I will use the previously 
## calculated version if available

cacheSolve <- function(x, ...) {
    alreadyCached <- x$getInverse()
    if(!is.null(alreadyCached)) {
        return(alreadyCached)
    }
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
}