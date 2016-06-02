## The first function creates an object that can cache the inverse of a matrix
## The second computes the inverse of the object generated in the first function. In the
## event that the inverse has already been stored in Cache, it should retrieve it from Cache and 
## not recalculate.

## This function creates a matrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(MatInverse) inv <<- MatInverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## this function will retrieve the matrix's inverse from the Cache, and if it is not available,
## it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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