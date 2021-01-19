# Matrix inversion is usually a costly computation. This pair
# of functions lets you cache the inverse rather than compute
# it repeatedly.

# Function 1: makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(aMat) {
                x <<- aMat
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(bMat) invMat <<- bMat
        getinverse <- function() invMat
        list(set=set, get=get,
             setinverse=setinverse, getinverse=getinverse)
}


# Function2: cacheSolve
# Computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If inverse has already been calculated,
# and the matrix has not been changed, it retrieves inverse
# from cache. Requires input of one argument which is an
# object of kind makeCacheMatrix.
cacheSolve <- function(x) {
        invMat <- x$getinverse()
        if(!is.null(invMat)) {
                message("Getting cached data")
                return(invMat)
        }
        theMat <- x$get()
        invMat <- solve(theMat)
        x$setinverse(invMat)
        invMat
}
