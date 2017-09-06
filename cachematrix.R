## These functions work in conjunction to provide
## a matrix implementation that caches the value
## of its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## the inverse
    i <- NULL
    ## function to set the value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## the function to get the value
    get <- function() {
        x
    }
    ## the function to set the inverse
    setinverse <- function(inverse) {
        i <<- inverse
    }
    ## the function to get the inverse
    getinverse <- function() {
        i
    }
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## returns the inverse of the passed matrix, returning
## a cached value or computing if this is the first call
## of the matrix has changed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## retrieve a possible cached version of the inverse matrix
    i <- x$getinverse()
    ## determine if we have a cached value
    if (!is.null(i)) {  
        ## if the value is not null, print a cache message
        message("returning cached data")
    } else {          
        ## if the value is null, compute it and cache the value
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
    }
    ## return the inverse of the matrix
    i
}
