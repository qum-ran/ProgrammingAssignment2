## `makeCacheMatrix`: Sets up a matrix "cache" to store its inverse, helping to speed up repeated calculations by avoiding unnecessary re-computation.
## `cacheSolve`: Checks if the matrix inverse is already in the cache and uses it; if not, it computes the inverse, saves it for future use, and returns it.


## Write a short comment describing this function
## initializes matrix and its inv.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(matrix){
		x <<- matrix
		inv <<- NULL
	get <- function() x
	
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	}



## Write a short comment describing this function
## cheks for a cached inverse first. if not, computes the inverse and returns it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)){
		message('getting cached data')
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv

}
