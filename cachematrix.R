## R Programming: Programming Assignment 2 
##
## This function creates a special "matrix" object that can cache its inverse.
## 
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- solve(x)
        getinverse <- function() s
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Solve function is computing the inverse of a square matrix.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data.")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setinverse(s)
        s
}

## Example:
## > x  <- matrix(1:4, 2, 2)
## > m  <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
##> cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
