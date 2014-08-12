##
## This file contains the work for Programming Assignment 2 of the R Programming course
##
## Since computing the inverse of a matrix can be computationally intensive, these functions
## provide for the caching and retrieval of the inverse of a square matrix
##
## Sample usage:
##      tm2 <- matrix(rnorm(100),10,10)
##      cm2 <- makeCacheMatrix(tm2)
##      cacheSolve(cm2)


#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## We don't check for basic errors such as non-square matrices, etc.
    ## For this assignment, assume that the matrix supplied is always invertible.
    minverse <- NULL
    
    # set re-initializes the object with a new matrix "y"
    set <- function(y) {
        x <<- y
        minverse <<- NULL
    }
    
    # return the original matrix
    get <- function() x
    
    # set the cached inverse to the given value
    setinverse <- function(inv) minverse <<- inv
    
    # return the local copy matrix inverse
    # This assumes it has been set via setinverse
    getinverse <- function() minverse
    
    # Helper function so that ls(makeCacheMatrix()) shows the callable functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the (possibly NULL) cached value of the inverse of the matrix "x"
    minv <- x$getinverse()
    
    # Return the cached value if found
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    
    # If it was NULL, we calculate it and store it in the object
    
    # Get the original matrix
    m <- x$get()
    
    # Find its inverse
    minv <- solve(m)
    
    # Cache the value of the inverse back in the object
    x$setinverse(minv)
    
    # Return the inverse to the caller
    minv
}

