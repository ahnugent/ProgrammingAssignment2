## makeCacheMatrix() and cacheSolve() are used together to computes and cache the inverse of a matrix,
## avoiding redundant computation.

## makeCacheMatrix() caches an input matrix as a list, with access methods:
##   set() assigns the input matrix to a cached object.
##   get() retrieves the cached input object (matrix).
##   setinverse() assigns the output (inverted) matrix.
##   getinverse() retrieves the output (inverted) matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() computes the inverse of the cached input "matrix", if the result is not current
## (as determined by testing for a null value of the cached result).

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    ## Return a matrix that is the inverse of 'x'
}
