## Functions for caching matrix inversion

## Test example
## matrix = makeCacheMatrix(rbind(c(10, 20), c(30, 40)))
## inv = cacheSolve(matrix)
## cachedInv = cacheSolve(matrix)

## Create list of functions to work with cached matrix
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

## Return the inverse of the matrix stored in a 'makeCacheMatrix' structure.
## Further calls to cacheSolve() for the same 'makeCacheMatrix' structure will return a cached inverse.
cacheSolve <- function(x, ...) {
    cached <- x$getinverse()
    if(!is.null(cached)) {
        # Return cached matrix if exists
        return(cached)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
