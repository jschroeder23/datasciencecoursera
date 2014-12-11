# ## Matrix inversion is usually a costly computation and their may be 
# some benefit to caching the inverse of a matrix rather than compute it 
# repeatedly . Our assignment is to write a pair of
# functions that cache the inverse of a matrix.

# ##  This function creates a special "matrix" object that can cache its 
# inverse.

makeCacheMatrix <- function(x = matrix()) {
  slv<- NULL
  set <- function(y) {
      x <<- y
      slv<<- NULL
  }
  get <- function()  x
  setInv <- function(solve) slv<<- solve
  getInv <- function() slv
  list(set = set, get = get,
      setInv = setInv,
      getInv = getInv)
}

# 
# ## This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  slv<- x$getInv()
  if(!is.null(slv)) {
      message("getting cached data")
      return(slv)
  }
  data <- x$get()
  slv<- solve(data, ...)
  x$setInv(slv)
  slv
}
