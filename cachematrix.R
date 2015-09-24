## Create two functions that are used to create a special 
## object that stores a matrix and caches its inverse matrix.

## makeCacheMatrix creates a special "vector", which is
## a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvm <- function(invm) m <<- invm
      getinvm <- function() m
      list(set = set, get = get,
           setinvm = setinvm,
           getinvm = getinvm)
      
}


## The following function calculates the inverse of the special
## matrix created with the above function. However, it first 
## checks to see if the inverse matrix has already been calculated. 
## If so, it `get`s the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the invserse matrix in the cache 
## via the `setinvm` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinvm()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinvm(m)
      m
}
