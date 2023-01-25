## makeCacheMatrix and cacheSolve are functions, which cahe the inverse of a matrix

## makeCacheMatrix is a function which creates a matrix object. This object caches
## the inverse for the input.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
			set <- function(y) {
			x <<- y
			inv <<- NULL	
		}
		get <- function() x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set=set,
			get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}



## cacheSolve is function, which computes the inverse of the matrix (latter is computed
## by makeCacheMatrix). If the inverse was already calculated, the cacheSolve retrieves
## the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        message("getting cached result")
        return(inv)	
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}