## makeCacheMatrix - special function that creates the matrix and can cache the inverse of it
## cacheSolve - computes the inverse of matrix creted by makeCacheMatrix function

## makeCacheMatrix returns a list
## list$set - set the value of the matrix
## list$get - get the value of the matrix
## list$setinv - cache the value of the inverse matrix
## list$getinv - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns an inverse of the matrix or cached value

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
