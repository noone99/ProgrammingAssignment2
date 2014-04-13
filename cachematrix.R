##
## Implements a set of functions that implement a set of funtions
##  that create a cacheable matrix [makeCacheMatrix()] and a 
##  function to solve for the inverse of that matrix and save it 
##  in the cache. Returning this cached value if called again.

##
## Create a cached matrix that includes functions to get a 
##  previously calculated inverse and a function to calculate
##  the inverse of the matrix and save that value in the cache.
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function checks if cached value exists for the matrix 
##  and returns it if present. If not, it calculates and saves
##  it in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
