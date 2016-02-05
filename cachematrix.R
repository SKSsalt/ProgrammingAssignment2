## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# x is a square invertible matrix
	# the result list is used for the cacheSolve() function

	m <- null
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # x is the result of the makeCacheMatrix() function

	m <- x$getInverse()
	# check if the inverse of the matrix is cached
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	# otherwise calculates the inverse
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
