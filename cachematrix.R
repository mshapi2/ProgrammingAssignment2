## A pair of functions `makeCacheMatrix` and `cacheSolve`
## that compute and cache the inverse of a matrix x.
## Example:
## x <- matrix(rnorm(1000000),1000,1000)
## xInv <- cacheSolve(makeCacheMatrix(x))

## `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.To do that,`makeCacheMatrix` 
## creates a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
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



## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the `setInverse`
## function.

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
