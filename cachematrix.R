## RPROG-002: peer reviewed assignment: using cache
## Steve Boender, April 22, 2014

## These two functions create a caching environment. By defining functions within functions
## R creates several stacked environments which can be used to store results of calculations. 


## The first function creates the environment. It is, essentially, identical to the Coursera 
## example, only using matrix and solve instead of vector and mean.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
		## several internal functions are defined, for setting and getting the actual input (*Matrix) and the cached results (*Inverse)
	setMatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
		## the output is a list. This way, you can call the main function with the internal function as arguments.
	list(setMatrix = setMatrix, 
		getMatrix = getMatrix,
		setInverse = setInverse,
		getInverse = getInverse)
}


## The second function checks the cache first (getInverse; is anything there?), 
## and if not, calculates the inverse and fills the cache using setInverse.
## This function is, again, not substantially different from the example. 
## The time spent on this assignment does not really show in this code. 

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
		## else: do the calculation and fill the cache for next time.
	data <- x$getMatrix()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
