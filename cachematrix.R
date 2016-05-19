## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly (there are 
## also alternatives to matrix inversion that we will not discuss here). Your assignment 
## is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
}

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}

# ## verification run
# x <- matrix(1:4, 2,2)
# m <- makeCacheMatrix(x)
# cacheSolve(m)
# cacheSolve(m)
