## This function  caches  the inverse of a matrix
## 

## The makeCacheMatrix creates a special "matrix" object that can cache
##   its Inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
                x <<- y
                inv <<- NULL
	}
      
	get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
   
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## The cacheSolve function computes the inverse of the 
##    special "matrix" returned by makeCacheMatrix if it 
##    has not been cached.

## If the inverse has been cached, it will return the cached value

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
 	
	inv <- x$getinv()
      
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
      }
      
	data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv

}
