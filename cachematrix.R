## These two functions in conjunction can create/display a matrix, and then calculate the inverse 
## of the matrix if it has not already been calculated. It can also store the value of the inverse 
## once calculated and reference this cached value later.


## The makeCacheMatrix function creates a list of four functions that will be referenced by the cacheSolve function.
## The functions stored within it can display or set the values of a matrix, and display or set the value of the inverse
## (but note that it does not acutally calculate the inverse, only store it.)

makeCacheMatrix <- function(x = matrix()) {
   v <- NULL 
    set <- function(y) {  ## this will change the value of x (the matrix) stored within this function to equal the value of y
      x <<- y             ## that you set in the parent environment
      v <<- NULL          ## then it sets the cached inverse to be null since the matrix values have changed
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function pulls from the first function, makeCacheMatrix to get the values of the matrix, and then calculates the
## inverse. First it checks to see if there is a cached value already stored there, and if so displays this instead of recalculating.

cacheSolve <- function(x, ...) {
    v <- x$getinverse()
    if(!is.null(v)) {               ## this checks to see if the value for inverse (v) has already been calculated
      message("getting cached data")
      return(v)
    }
    data <- x$get()               ## this pulls the contents of the matrix using the get function in makeCacheMatrix
    v <- solve(data, ...)         ## this actually computes the inverse of the matrix
    x$setinverse(v)               ## this stores the value of the inverse that was just calculated in the makeCacheMatrix function
    v
}
