makeCacheMatrix <- function(x = array()) {
	i <- NULL
        
	set <- function(y) {

		x <<- y

		i <<- NULL
	}
        
	get <- function() x
        
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
        
	list(set = set, get = get,

		setinverse = setinverse,
	getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        
	i <- x$getinverse()
        
	if(!is.null(i)) {
                
		message("getting cached data")
		return(i)
        
	}
        
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

## Testing the caching

b <- matrix(c(1,2,3,4), 2, 2)
b1 <- makeCacheMatrix(b)
cacheSolve(b1)