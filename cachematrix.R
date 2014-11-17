## In order to speed up repeated calculation of matrix inverses, the
##  makeCacheMatrix/cacheSolve functions provide a new matrix object that
##  will automatically cache the expensive computation of a matrix's inverse.
##
## Usage: 1) Create a cachable matrix by calling makeCacheMatrix and passing 
##             in an invertable matrix.
##        2) Call cacheSolve on the resulting list to get the matrix's inverse.
##             This step can be repeated any number of times and the inverse will
##             only ever be created once.
##
## Example: 1) m <- makeCacheMatrix(matrix(c(1, 2, 2, 2), 2, 2))
##          2) cacheSolve(m)
##
## Other functionality:
##          1) Get or set the underlying matrix using m$get() and m$set(matrix),
##               respectively.  Setting the matrix resets the cached inverse.

## Creates a matrix which can store a cached copy of its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # The cached inverse of our matrix.
    inverse <- NULL
    
    # Getter and setter for the matrix itself.
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL ## clear the cached value so we can recompute it
    }

    # Getters and setters for the inverse of the matrix.  These should not
    #   be called directly and are only for use by cacheSolve()
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    # Return list with our 4 functions as elements with the same name.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of a matrix created with makeCacheMatrix.
## 
## If available, a cached copy of the inverse will be returned.  Otherwise, 
##  computes and caches the inverse before returning it.
cacheSolve <- function(x, ...) {
    ## See if we've already cached the inverse of x
    inv <- x$getInverse()
    if (!is.null(inv)) {
        ## Already cached, just return it
        message("Using cached inverse")
        return(inv)
    }
    
    ## The inverse wasn't cached, so compute it, cache it, and then return it
    inv <- solve(m$get())
    m$setInverse(inv)
    inv
}
