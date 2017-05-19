## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Get square invertible matrix
  # Later use it as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # Assign a value to an object in an environment 
    # different from the current environment, using '<<' 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## Use output of makeCacheMatrix() function
  ## Return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated, get it from cache and skip calculation
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # Else, calculate the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # Set the value of the inverse in the cache.
  x$setinv(inv)
  
  return(inv)
}
