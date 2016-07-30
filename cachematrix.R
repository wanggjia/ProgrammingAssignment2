## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special matrix object which can cache the inverse of x. 
## The funciton returns a list which are the functions for get and set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invers <- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
    matequal <- function(x, y){
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    }
    set <- function(y) {
        if (! matequal(x,y)){
            x<<- y
            invers <<- matrix(nrow = dim(y)[1], ncol = dim(y)[2])
        }
    }
    get <- function() x
    setinverse <- function(inv) invers <<- inv
    getinverse <- function() invers
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the matrix
## If the inverse exists and the matrix has not been changed, the cache will be returned
## Otherwise, the inverse will be calculated, put to cache and returned. 

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        if (all(is.na(m))){
            invers <- solve(x$get())
            x$setinverse(invers)
            invers
        }
        else {
            m
        }        
}
