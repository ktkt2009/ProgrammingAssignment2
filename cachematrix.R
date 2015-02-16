## Creating a special matrix that can cache its inverse
## It is concerned with lexical scoping and caching functions
# makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  matX <- NULL
  set <- function(y) {
    x <<- y
    matX <<- NULL
  }
  get <- function() {           # get return matrix
    x
  }
  setSolve <- function(solve) { # Function to get the inversed matrix from a special object created by makeCacheMatrix.
    matX <<- solve
  }
  getSolve <- function() {      # return cache inverse matrix 
    matX
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)  #Returned object (actually it's a list) contains methods:
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  matX <- x$getSolve()
  if(!is.null(matX)) {
    message("getting cached data")
    return(matX)
  }
  data <- x$get()
  matX <- solve(data, ...)
  x$setSolve(matX)
  matX              #  invesres matrix from the cache
}
