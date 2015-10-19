## The function makeCacheMatrix defines a special type of matrix comprising list of functions
## to cache specified matrix and access its inverse.
## The inverse of the matrix is computed by the function cacheSolve.
## Subsequent invocation simply returns the cached inverse until it is reset.

## This function returns a list of functions which can operate on the cached matrix
## Usage: makeCacheMatrix(simpleMatrix)
## Arguments: x, the matrix to be cached and decorated with special cache operations
## Returns: special matrix that has following functions:
## - set: updates the underlying matrix instance
## - get: gets the cached matrix
## - setinverse: sets the inverse of the matrix
## - getinverse: gets the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	# cache the matrix into x and reset the cached inverse to NULL
	set <- function(y) {  	#update cache for matrix and invalidate inverse cache
		x <<- y           
		inverse <<- NULL
	}
	
	# returns cahced matrix
	get <- function() x

	# updates the inverse cache with the specified matrix
	setinverse <- function(i) inverse <<- i

	# returns the cached copy of inverse
	getinverse <- function() inverse

	# constructs and returns list of functions to work with
	# cachable matrix instance
	list(set = set, get = get, setinverse = setinverse, 
		getinverse = getinverse)
}


## This function returns cached inverse of the specified square matrix. If the inverse is not cached yet,
## it will be computed and cached.
## Usage: cacheSolve(cacheMatrix)
## Arguments: x, cacheMatrix to be inversed and cached. x must be a square matrix.
## Returns: inverse of the cached matrix
cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	if(!is.null(inverse)) { #use cached inverse if one exists
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inv <- solve(data, ...)	
	x$setinverse(inv)		#update cache
	inv	
}
