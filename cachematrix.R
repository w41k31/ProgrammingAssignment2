## Matrix inversion caching implementation
## 
## Example of usage:
## 
## matr <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
## cacheableMatr <- makeCacheMatrix(matr)
## system.time(cacheSolve(cacheableMatr)) ## initial computation
## system.time(cacheSolve(cacheableMatr)) ## check second time to see the difference

## This function creates special matrix for caching purposes

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function figures out if inverse value is already calculated and if so - returns it, otherwise - calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
