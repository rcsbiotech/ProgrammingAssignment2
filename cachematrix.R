## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Stores the inverse of a given matrix in cached memory,
# allowing quickly retrieval without the necessity of recalculating the matrix
# itself.

## [.. Usage ..]
### 1. Creates a matrix
# x <- matrix(round(rnorm(9, 3, 2)), 3, 3)
### 2. Stores the inverse on a special constructor
# inverse.x <- makeCacheMatrix(x)
### 3. Recovers the cached inverse without recalculating anything
# cacheSolve(inverse.x)


## Functions:
# makeCacheMatrix: a constructor for a function
# that stores the inverse of a function in cache in its first calculation

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## Functions:
# cacheSolve: retrieves a previously cached, inverse matrix from a construct
# built with makeCacheMatrix()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


