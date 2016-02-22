#           # makeMatricCache is a special matrix that can cache its inverse.
            # cacheSolve will look to see if the inverse has been solved and return, 
            # otherwise it will solve, store, and return
## functions do

## makeCacheMatrix ix a matrix that can store its own inverse.  This is useful for large matrices 
## where computing the inverse can be expensive

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix made with the above function and returns its inverse.  What is nice is
## if teh inverse has been calculated before, it returns the stored inverse, otherwise it caclulates.
## stores for future calls, and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
