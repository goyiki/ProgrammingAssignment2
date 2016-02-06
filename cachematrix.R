### This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<-NULL
  }
  get<- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

### This function calculates the inverse of a matrix returned by makeCacheMatrix.
# If this computation has been performed before, it retrives it (instead of computing it again).
# Otherwise it calculates it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message ("Getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }

###########################################################################################################################################
#  Here an example: 
mat <- matrix(rnorm(64), nrow=8, ncol =8)   # Generation of a matrix called "mat"
mat_inv <- makeCacheMatrix(mat)            
cacheSolve(mat_inv)                 # It computes the inverse of "the matrix "mat"
cacheSolve(mat_inv)                 # It gets the inverse from the cache (instead of making the computation again). See message displayed
###########################################################################################################################################

