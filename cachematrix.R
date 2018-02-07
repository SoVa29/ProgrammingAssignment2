## ProgrammingAssignmentWeek3
#
# Caching the Inverse of a Matrix
#
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$setInverse(inv)
  inv
}

##    TEST
#
## mat <- matrix(c(1:4),c(2,2))
## mat 
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
## my_matrix <- makeCacheMatrix(mat)
## my_matrix$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## cacheSolve(my_matrix)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
## cacheSolve(my_matrix)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
## my_matrix$getInverse()
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#

