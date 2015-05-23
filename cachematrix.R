## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# Initializing output variable 'inv' to NULL
	inv <- NULL	
	
	# 
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	# Returns matrix
	get <- function() x
	# Calculates matrix inverse and stores in cache
	setinv <- function(m) inv <<- m
	# Returns inverse stored in cache
	getinv <- function() inv
	
	# Returns list
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

	# Fetches result stored in cache if the cache is not NULL
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# Fetches matrix if cache is NULL
	data <- x$get()
	# Calculates inverse ...
	inv <- solve(data, ...)
	# ... and saves in cache	
	x$setinv(inv)
	inv	
}
