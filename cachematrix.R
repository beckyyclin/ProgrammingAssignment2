##### Assignment 2: Lexical Scoping #####
## Student: Yi-Chen (Becky) Lin
## Version: May 23 2019
## Purpose: writing cache the inverse of a matrix
## Reasoning: Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix 
# rather than computing it repeatedly


##### <1> makeCacheMatrix Function #####
# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inv <<- inverseMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##### <2> cacheSolve Function #####
# a.    This function computes the inverse of the special
#       "matrix" returned by `makeCacheMatrix` above. If the inverse has
#       already been calculated (and the matrix has not changed), then
#       `cacheSolve` should retrieve the inverse from the cache.
# b.    Computing the inverse of a square matrix can be done with the 
#       `solve` function in R.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}


