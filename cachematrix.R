## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
