## A set of functions which store the value of a matrix within a cache, and retrieves said data if cached or
## calculates the inversion of that matrix.

## Set and get the value of a matrix, and set and get the value of the inversion of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsol <- function(solve) inv <<- solve
    getsol <- function() inv
    list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## Gets the inversion of a matrix, or retrieves cached matrix data.

cacheSolve <- function(x, ...) {
    inv <- x$getsol()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsol(inv)
    return(inv)
}
