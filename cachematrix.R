## Get the inverse of a matrix previously calculated

## 'makeCacheMatrix' create a matrix that can set its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ## set/get functions work with the matrix x and setinv/getinv work with the inverse
  ## of the matrix x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## 'cacheSolve' calculates the inverse of a matrix created by 'makeCacheMatrix'
## or gets it from cache

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  ##Check if the inverse of the object x has been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## Notice that the inverse was not in cache, so we calculate this
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
