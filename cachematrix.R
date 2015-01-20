# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. These
# two functions will cache the inverse of a matrix.

# makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# cacheSolve computes the inverse of the special matrix (that was returned by
# makeCacheMatrix), unless the inverse has already been calculated (and the
# matrix has not changed), and then the cachesolve should retrieve the inverse
# from the cache.

cacheSolve <- function (x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  
}
