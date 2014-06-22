## The formulas that follow are nearly identical to the ones
## provided in the course example. I only modified the names
## of the objects and change the mean() function to
## the solve() function and add comments.

## These two functions, identical to the fuctions makeVector and
## cachemean provided in the example but with different names,
## are used together to cache the value of the inverse of a matrix
## so that it only recalculates when needed. 
## Use the functions by first storing an invertible matrix in the
## object created by makeCacheMatrix. (e.g. given a matrix 'z',
## matrix <- makeCacheMatrix(z)). Then pass matrix to the
## cacheSolve function to calculate the inverse. After passing it
## once, subsequent calls to cacheSolve passing matrix will return
## a note saying that it is returning the cached value. You can
## update the matrix being used by calling the function 
## matrix$set(newmatrix), or you can create a new matrix object
## using makeCacheMatrix. The matrix object will store the inverse
## until the matrix$set(newmatrix) command is called. At that point
## the first call to cachesolve passing matrix will calculate the
## new inverse.

## Creates a new matrix object that will store a matrix, it's
## inverse, and provide methods to update both.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Accepts an object created by createCacheMatrix. Returns the 
## inverse of the matrix stored in createCacheMatrix. If the
## inverse has already been calculated, return the cached value.
## Otherwise return the newly calculated value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
