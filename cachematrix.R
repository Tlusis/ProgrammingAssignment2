## Pair of functions that saves the inverese of matrix in a cache for retrieval.

## This function creates the matrix which inverse should be cached.

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


## This functions checks if matrix inverse is saved in cache, if not then solves it.

cacheSolve <- function(x, ...) {
        {
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
        ## Return a matrix that is the inverse of 'x'
}
