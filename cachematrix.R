## Matrix inversion is usually a costly computation
## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## The functions defined are:
## get -- returns the matrix
## setinverse -- set the inverse matrix 
## getinverse -- get the inverse of matrix, if cached returns the inverse matrix or else return NULL

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns inverse of matrix from cache if it is present or else calculates the 
## inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(is.null(inv)) {
                data <- x$get()
                inv <- solve(data)
                x$setinverse(inv)
        }
        inv
}

## for testing
# mat = rbind(c(4, 3), c(3, 2)) 
# m = makeCacheMatrix(mat)
# i = cacheSolve(m)
# i
