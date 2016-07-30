## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
