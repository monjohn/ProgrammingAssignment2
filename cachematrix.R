## functions Invert a matrix, caching the result to save processng time and resources
## if the same matrix needs to be inverted repeatedly


## function stores the ability to compute and cache the inverse of a matrix,
## returns a list of functions that close over a matrix and previous inversion
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Takes a cached matrix, checks if it was previously inverted, if so, 
## returns cached result, otherwise computes the inverse.
cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached value for inverse")
    inverse
  }
  data<-x$get()
  inverse<- solve(data,...)
  x$setinverse(inverse)
  inverse
}

