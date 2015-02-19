## The collection of functions to solve matrix inversion 
## using the cache
## If you want check this code, try:
## > A = matrix(c(1:4), nrow = 2)
## > ac <- makeCacheMatrix(A)
## > cacheSolve(ac)

## This function creats a vector which contain methods to 
## get/set the matrix inverse
## x param – the original martix

makeCacheMatrix <- function(x = matrix()) {
	i_m <- NULL
	set <- function(y) {
		x <<- y
		i_m <<- NULL
	}
	get <- function() x
	setInverted <- function(inverted) i_m <<- inverted
	getInverted <- function() i_m
	list(set = set, get = get,
		 setInverted = setInverted,
		 getInverted = getInverted)
}

## Solve the matrix inversion, but using a cache object
## Check if the cached inverse exists or calculate it and store in the cache
## x param - the object returned from the makeCacheMatrix(matrix) function

cacheSolve <- function(x, ...) {
	i_m <- x$getInverted()
	if (!is.null(i_m)) {
		message("getting cached data")
		return(i_m)
	}
	i_m <- solve(x$get(), ...)
	x$setInverted(i_m)
	i_m
}
