
## Below are two functions that are used to create a special object that stores a numeric matrix and caches its inverse.

## The function below creates a list containing functions to
#1.  set the value of a matrix
#2.  get the value of a matrix
#3.  set the value of the inversion of the matrix
#4.  get the value of the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
	x<<-y
	inv<<-NULL
    }
    get <- function() x
    setinverse<-function(invers) inv<<-invers
    getinverse<-function() inv
    list(set = set, get = get,
	    setinverse = setinverse,
	    getinverse = getinverse)

}


## This function calculate the inversion of the object created by the function above.
## If the cached value exists, use it. If not, calculate it and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
    }
    data <- x$get()
    inv <- solve(data);
    x$setinverse(inv)
    inv
}
