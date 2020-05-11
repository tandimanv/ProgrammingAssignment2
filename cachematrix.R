## These functions allow to set and store a matrix and its inverse, so that
## the calculated inverse is retrievable

## The first function below generates a list of functions that set and get 
## an input matrix, and also set and get a matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL 
      set <- function(y) {   
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## The following function checks if there's been a stored value for a matrix 
## inverse and calls that value. Otherwise it calculates the inverse of 
## the input matrix.

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
