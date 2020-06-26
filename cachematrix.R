## Put comments here that give an overall description of what your
## functions do
################
## These two functions are used to create a special Matrix that can
## save its own inverse.
################

## Write a short comment describing this function
################
## This function is used to create our special matrix with its 
## inner methods to set and to get values, such as its own
## value and its inverse
################
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinv <- function(inv) im <<- inv
  getinv <- function() im
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
################
## This function is used to calculate the inverse of our
## special matrix, if it does not have it
################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinv(im)
  im
}
 