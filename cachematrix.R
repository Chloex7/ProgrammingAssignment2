## This function creates a matrix and its inverse, which will be cached

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## and will retrieve the cached version if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- inverse(data, ...)
  x$setinverse(a)
  a
}
