## They speed up the process of inverting the matrix by caching it


## sets the values of the special matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) m <<- Inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## gets the inverse of the matrix

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
       ## Return a matrix that is the inverse of 'x'
}
