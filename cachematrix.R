## Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation.  These functions
## expose a matrix inversion function called cacheSolve which will
## cache the inverse of a matrix so that it does not to be computed
## repeatedly.
##
## Usage:
## > mc <- makeCacheMatrix(matrix(...))
## > cacheSolve(mc)
## (Subsequent calls of cacheSolve(mc) will return the cached result)
##
## @author Steven Engelhardt
## @date   2014-04-27

## Given a matrix, create and return a new matrix-like object that
## can cache its inverse.
##
## @param  x a matrix which will be wrapped by the function 
## @return   a matrix-wrapping object which includes methods to
##           get/set data and get/set the cached inverse
## @see      cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
	# get() returns the matrix encapsulated by the object
	get <- function() x
	# set() clears both the data and the cached inverse
	set <- function(y) {
	    x <<- y
		inv  <<- NULL
	}
	# getInverse() returns the cached inverse
	getInverse <- function() inv
	# setInverse() saves the inverse to the cache
	setInverse <- function(inverse) inv <<- inverse
	list(set = set,
	     get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## Given a matrix object returned by makeCacheMatrix(), either returned
## the cached inverse or calculate the inverse and cache it.
##
## @param  x    a matrix object returned by makeCacheMatrix()
## @param  ...  any additional arguments to the solve() function
## @return      the inverse of the matrix, which may have been calculated
##              on the fly or returned from a cache
## @see         makeCacheMatrix
cacheSolve <- function(x, ...) {
    # return the inverse from the cache if it has been cached already
    inverse <- x$getInverse()
	if (!is.null(inverse)) {
	    message("getting cached data")
		return(inverse)
	}
	# calculate the inverse, cache it, and return it
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
