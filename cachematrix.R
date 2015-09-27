## A combination of these two functions allow cached computation of matrix inverse

## Write a short comment describing this function
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


## Write a short comment describing this function
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    #message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
