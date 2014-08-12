##
## This file contains the work for Programming assignment 2 of the R Programming course
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

# Test cases
tm1<-matrix(c(c(2:4),c(1,3,7),c(4,7,13)),nrow=3,byrow=TRUE)
print(solve(tm1))

tm2 <- matrix(rnorm(100),10,10)
print(solve(tm2))


# 1.    Was a valid GitHub URL containing a git repository submitted?
# 2.    Does the GitHub repository contain at least one commit beyond the original fork?
# 3.    Was a SHA-1 submitted indicating a specific commit in the GitHub repository?
# 4.    If a SHA-1 was submitted along with the GitHub URL, does the SHA-1
#       correspond to a specific commit in the repository?

# Overall evaluation/feedback
# 1.    Does the GitHub repository contain an R file containing code implementing the completed assignment? 
# 2.    Does the R file containing the code have any comments explaining what
#       the code does? NOTE: The makeCacheMatrix and cacheSolve functions should both
#       be documented with explanatory comments. There maybe other functions in the R
#       file but they do not need to be commented.
# 3.    Does the R code implementing the 'makeCacheMatrix' function appear to be
#       correct, to the best of your ability to judge?
# 4.    Does the R code implementing the 'cacheSolve' function appear to be
#       correct, to the best of your ability to judge?

# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }
