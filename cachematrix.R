########################################################################
### Example: Caching the Mean of a Vector


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
    setmean = setmean,
    getmean = getmean)
}


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}



#######################################################################
### Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). For this assignment, assume that the matrix supplied
# is always invertible.



# This function creates a special "matrix" object that can cache its inverse.
# Structure is from example (caching the mean of a vector), substituted with inverse


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <-- NULL
    }
    
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}



# This function computes the inverse of the special "matrix" returned by
# `makeCacheMatrix` above. If the inverse has already been calculated
# (and the matrix has not changed), then `cacheSolve` should retrieve the
# inverse from the cache.

# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.


cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    ## Return a matrix that is the inverse of 'x'
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
    
}



