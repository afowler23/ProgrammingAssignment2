# The inversion of a matrix is usually a time-consuming computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions below are used to cache the inverse of a matrix.


# makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

# 1. Set the value of the matrix

# 2. Get the value of the matrix

# 3. Set the value of the inverse

# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 invs <- NULL
  set <- function(b) {
    x <<- b
    invs <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invs <<- inverse
  getinv <- function() invs
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        invs <- x$getinv()
  if (!is.null(invs)) {
    message("Getting cached data...")
    return(invs)
  }
  data1 <- x$get()
  invs <- solve(data1, ...)
  x$setinv(invs)
  invs
}
