@@ -1,15 +1,34 @@
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These functions look to see if there is already a 
## cached, inverted matrix in memory, so that it does
## not have to calculate it more than once.

## Returns a list of 4 functions, allow you to set and get 
## matrix and matrix inverse values.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## Returns the matrix inverse value. If value is already
## calculated, returns cached value.
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
