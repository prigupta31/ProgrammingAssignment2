## These two functions allow for one to store and retrieve matrices as well
## their inverses

## makeCacheMatrix hosts four functions related to calulating a matrix inverse
## set() sets and stores the matrix data
## get() retrieves stored matrix data
## setInverse() is meant to enable solving for the inverse of the matrix
## getInverse() is meant to enable retrieving the inverse of a matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function (matrix) m <<- matrix
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks whether the inverse of the matrix provided is already stored.
## If so, the function retrieves the inverse from the cache and prints it. If not,
##  calculates, caches, and then prints the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
