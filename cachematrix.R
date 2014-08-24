## The following functions are computing the inverse of a matrix 
## and able to cache the result of it. 

## This function creates a "special" matrix that is computing and caching the 
## inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {inv <<- solve}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## From the special matrix created by the above function, the following 
## function computes the inverse of the matrix if it has not been cached. 
## If it has, return the cached inverse. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
