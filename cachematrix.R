## build matrix objects that can cache their inverse when
## it is computed, so as to avoid repeating costly computations

## makeCacheMatrix builds a matrix object with caching 
## enabling it to store the value of its inverse
makeCacheMatrix <- function(x = matrix()) {

    ## attributes
    # s variable stores the cached matrix inverse
    s <- NULL
    
    ## getters
    # getter for original matrix
    get <- function() x
    # getter for cached inverse matrix
    getsolve <- function() s

    ## setters
    # setter for the matrix data, (resets the cached inverse)
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    # setter for the computed inverse
    setsolve <- function(solve) s <<- solve
    
    # return the "enhanced" matrix object (with inverse caching feature)
    list(
        set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


## cacheSolve finds the specified matrix object's inverse
## * retrieves it from the cache if it has already been computed
## * otherwise, compute it and cache it
## x should be a special matrix object built with makeCacheMatrix
cacheSolve <- function(x, ...) {

    # get invers from enhanced matrix object's cache
    s <- x$getsolve()

    if (!is.null(s)) {
        # inverse was found in the cache, just return it

        message("getting cached data")
        
    } else {
        # inverse was not found in the cache, compute it, cache it and return it

        # get original matrix
        data <- x$get()
        # compute inverse matrix
        s <- solve(data, ...)
        # store computed inverse in enhanced matrix object's cache
        x$setsolve(s)
    }

    # return inverse matrix
    s
}
