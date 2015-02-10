## Creating a special matrix that can cache its invese
## It is concerned with lexical scoping and caching functions
# makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  matt <- NULL
  set <- function(y) {
    x <<- y
    matt <<- NULL
  }
  get <- function() {           # get return matrix
    x
  }
  setSolve <- function(solve) { # Function to get the inversed matrix from a special object created by makeCacheMatrix.
    matt <<- solve
  }
  getSolve <- function() {      # return cache inverse matrix 
    matt
  }
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)  #Returned object (actually it's a list) contains methods:
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  matt <- x$getSolve()
  if(!is.null(matt)) {
    message("getting cached data")
    return(matt)
  }
  data <- x$get()
  matt <- solve(data, ...)
  x$setSolve(matt)
  matt
}
