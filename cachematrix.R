#the below function creates a matrix 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setval <- function(mean) m <<- mean
        getval <- function() m
        list(set = set, get = get,
             setval = setval,
             getval = getval)
}
# the below function provides the cache value of the matrix if avaliable or else computes the new value
# and returns it

cacheSolve <- function(x, ...) {
        m <- x$getval()                             #query the x vector's cache
        if(!is.null(m)) {                           #checks for cache
                message("getting cached data")
                return(m)                           #returns the avaliable cache value
        }
        data <- x$get()                             #compute if no value in cache
        m <- solve(data, ...)                       # get the new inverse of the matrix
        x$setval(m)
        m                                           #return the result
}
