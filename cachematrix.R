## The following pair of functions can be used to cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the inverse is retrieved from the cache, thus allowing the programmer
## to avoid costly computations.
## N.B. Both functions assume that the matrix supplied is always invertible.
## This function creates a special "matrix" object that can cache its inverse.

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
## Function calculates the inversial matrix of the special "matrix" object.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
