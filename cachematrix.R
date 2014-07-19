## makeCacheMatrix -- creates a cached matrix and sets up associated functions 
# for cached operations. Takes a matrix as input.
#
## cacheSolve -- solves for the inverse of a matrix, if a cached matrix exists
# the data from the cache is retured.

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    list(set=set, get=get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if (!is.null(m)) {
      message("Getting cached data.")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
