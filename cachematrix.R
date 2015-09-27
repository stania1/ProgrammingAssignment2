## A combination of these two functions allow cached computation of matrix inverse

## Returns a matrix with some wrapper functions
## (a getter and setter for the matrix itself and for the cached inverse)
makeCacheMatrix <- function(x = matrix()) {
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


## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


# helper functions for benchmarking (not part of the assignment)
# TODO make them DRY

benchmarkCacheSolve <- function() {
  system.time(manyCacheSolve())
}

manyCacheSolve <- function() {
  a <- matrix(c(10,-9,-12,7,-12,11,-10,10,3), ncol=3, nrow=3)
  cache_matrix <- makeCacheMatrix(a)
  for (i in 1:100000) {
    cacheSolve(cache_matrix)
  }
}

benchmarkNoCacheSolve <- function() {
  system.time(manyNoCacheSolve())
}

manyNoCacheSolve <- function() {
  a <- matrix(c(10,-9,-12,7,-12,11,-10,10,3), ncol=3, nrow=3)
  cache_matrix <- makeCacheMatrix(a)
  for (i in 1:100000) {
    solve(cache_matrix$get())
  }
}
