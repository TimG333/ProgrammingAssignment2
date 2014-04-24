## As per the assignment the functions create and solve a matrix that allows
## for a cached solution

## Creates a list of functions to allow the caching to work

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrixinverse <<- inverse
  getInverse <- function() matrixinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Uses the list from makeCacheMatrix to solve or retrieve a Matrix

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getInverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  } 
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setInverse(matrixinverse)
  matrixinverse
}
