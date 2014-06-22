## This functions are used to calculate the inverse of a matrix and to save the 
## result in a cache object.
## 2014/06/22 by MonicaPH for the R programming Coursera course.

## makeCacheMatrix constructs a cached matrix object. Its attributes are the matrix and its inverse.
## getters and setters are provided for these attributes

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve calculates the inverse of a cached matrix, stores the result in the
## corresponding attribute, returns this value and shows the result in console. If 
## the inverse was previously calculated, then the already calculated inverse is 
## printed and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
