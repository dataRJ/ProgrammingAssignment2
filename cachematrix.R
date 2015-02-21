## makeCacheMatrix is used to create a special matrix object that will be able to cache it's inverse.

## cacheSolve is used to take a matrix created using the makCacheMatrix to try and reuse a cached instance
## of the inverse of the provided matrix.  If not already created it will create the inverse and cache the 
## result


## makeCacheMatrix
## This object has the following functions:
## set - to set the matrix object
## get - to get the matrix object, NULL if not set
## setInverse - to cache the inverse of the matrix object
## getInverse - to get the cached inverse of the matrix object, NULL if not set

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setInverse <- function(inverse) m<<-inverse
        getInverse <- function() m
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
        
}


## cacheSolve
## This function takes the result of makeCacheMatrix and will retieve the inverse from the cache
## or it will calculate the inverse if the cache is empty and store it in the cache.  The resulting
## inverse will be returned in either case.
## Please note that this function assumes that the matrix being provided is invertable.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
