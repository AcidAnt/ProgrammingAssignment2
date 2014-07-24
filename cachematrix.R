## As matrix inversion is a very costly operation, these functions
## help caching a matrix's inverse by taking advantage of of R 
## scoping capabilities.


## Returns functions for getting and setting a matrix 'x' and its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <<- NULL
  
  # functions for setting and getting the matrix, 
  # inverted matrix is initially NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  
  # functions for setting and getting the inverted matrix
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(
    set = set, get = get,
    setinv = setinv, getinv = getinv
  )
}


## Returns a matrix that is the inverse of a cacheMatrix 'x'.
## If the inverse is already cached, the cache value is returned
## otherwise the value is calculated and gets cached.
cacheSolve <- function(x, ...) {
  
  # check if inverse is cached and return it
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached matrix ...")
    return(inv)
  }
  
  # inverse is not cached: calculate and cache it
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  
  inv
}
