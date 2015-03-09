## These two functions effectively store the value of a matrix and caches its
## inverse so that it can be retreived if and when required.

## This function is a set of four functions that lay the base for computation
## of the inverse and storing of the result.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function checks for stored data. If there is no stored data, it
## computes the inverse and then stores the result in the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i

}
