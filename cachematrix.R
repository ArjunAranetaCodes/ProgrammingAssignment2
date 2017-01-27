### Assignment: Caching the Inverse of a Matrix ###
### Matrix inversion is usually a costly computation and there may be some 
### benefit to caching the inverse of a matrix rather than compute it repeatedly. ###
### This program contains two functions that uses a special object that 
### stores a matrix and caches its inverse. ###

### Below is a matrix object which is able to cache its inverse ###

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


### The function below basically computes the special matrix from the
### makeCacheMatrix above. If the inverse is already been done or computed
## and the matrix remains the same, the it will retrieve the inverse.

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
