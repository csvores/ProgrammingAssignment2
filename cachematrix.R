## This file updated 18 November - csvores

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv  <- NULL
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) inv  <<- inverse
        getinverse  <- function() inv
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix created by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
