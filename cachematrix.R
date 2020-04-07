## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# cache matrix in X

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  # check if calculate
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  # calculate inverse
  inv = solve(mat.data, ...)
  
  # sets  the inverse in the cache 
  x$setinv(inv)
  
  return(inv)
}
