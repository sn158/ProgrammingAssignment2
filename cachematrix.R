## The function 'makeCacheMatrix' is s list a functiom to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the special "matrix"
## 4. get the value of the inverse of the special "matrix"
## functions do

## The function 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  ## Set the value of the matrix and cache's its reverse
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  ## Set the value of the inverse matrix
  setInverse <- function(inverse) inverse_matrix <<- inverse
  ## Get the value of the invese matrix
  getInverse <-function() inverse_matrix
  ## Creates a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function 'cacheSolve' computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix'
## If the inverse has already been calculated,
## then the 'cacheSolve' retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x',
  ## but first checks if the inverse matrix has already been
  ## calculated and if so gets the inverse form the cache 
  ## and skips the computation.
  inverse_m <- x$getInverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  ## Calculates the inverse matrix of the special "matrix"
  ## if it has not been done before
  ## and sets the value in the cache via the setInverse function.
  data <- x$get()
  inverse_m <- solve(data, ...)
  x$setInverse(inverse_m)
  inverse_m
}
