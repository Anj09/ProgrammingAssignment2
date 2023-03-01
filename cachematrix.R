## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: The makeCacheMatrix function takes an optional argument x that defaults to an 
## empty matrix. It creates a list object that has four functions: set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)       

}


## Write a short comment describing this function : The cacheSolve function takes a matrix object created by makeCacheMatrix as its first argument, 
## and any additional arguments are passed on to the solve function. 
## It first checks if the inverse has already been cached by calling getInverse on the matrix object.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Receiving the cached data")
    return(inv)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
