##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

##Write the following functions:
        
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

##For this assignment, assume that the matrix supplied is always invertible.

##The following is my makeCacheMatrix function for Programming Assignment 2

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){ ## set value of matrix
        x <<- y
        m <<- NULL
}
get <- function() x ##get value of matrix
setinverse <- function(solve) m <<- solve ##set value of inverse matrix using solve
getinverse <- function() m ##get value of inverse matrix
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following is my cacheSolve function to compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
