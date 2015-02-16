## cachematrix.R
##
## Construct a matrix object that caches value of its inverse to avoid
## the overhead of recomputing it.

## Construct the matrix. This function assigns the initial value of the matrix,
## and defines further functions that take care of the housekeeping duties
## (getting and setting the values of the matrix itself and of the inverse).
##
## The object returned is really just a list of these housekeeping functions,
## but due to lexical scoping, each of them has access to the state variables 
## (inv and x) via the parent environment, makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    # initialize the matrix inverse
    inv <- NULL
    # set the value of the matrix
    set <- function(y) {
        # assign a new value, accessing the parent environment
        x <<- y
        # reinitialize the matrix inverse so that we don't retrieve out-of-date
        # cached data
        inv <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the value of the matrix inverse
    setinverse <- function(inverse) inv <<- inverse
    # get the value of the matrix inverse
    getinverse <- function() inv
    # return the list of methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Retrieve the inverse of the matrix, either by calculating it and caching it
## if we haven't yet done so, or retrieving it if we have already calculated and
## cached it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get the current value of inv
    inv <- x$getinverse()
    # if it is not NULL, we can retrieve the cached value
    if(!is.null(inv)) {
        # report that we're retrieving cached data
        message("getting cached data")
        # ... and return the value
        return(inv)
    }
    # otherwise, we need to solve for the inverse
    # first, get the matrix
    data <- x$get()
    # ... then invert it
    inv <- solve(data, ...)
    # ... store the inverse
    x$setinverse(inv)
    # ... and return
    inv
}
