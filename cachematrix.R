## Put comments here that give an overall description of what your
## functions do
## These functions are for demonstrating the use of caching values instead of repeadtly computing potentially long
## running computations.  Using cached values can save lots of time and processing power.

## Function: makeCacheMatrix
## This funciton does the storing of the matrix, and the caching of the matrix value computed.
## Note that it does not actually compute the cached value it only stores.
## functions available: set, get, setInverseCache, getInverseCache

makeCacheMatrix <- function(x = matrix()) {
  #Create cache value
  inverseCache <- NULL
  
  #Store new matrix
  setMatrix <- function(newMatrix) {
      x <<- newMatrix
      #reset cache value
      inverseCache <<- NULL
  }
  
  #Return stored matrix
  getMatrix <- function() x
  
  #Store cache value
  setInverseCache <- function(inverse) inverseCache <<- inverse
  #Get cache value
  getInverseCache <- function() inverseCache
  
  #Store the functions in function makeCacheMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseCache = setInverseCache,
       getInverseCache = getInverseCache)
}


## Function cacheSolve
## This function exercises the makeCacheMatrix functions. It will attempt to use the cache, or compute and store
## the value if it is not available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getInverseCache()
      
      #Check to see if cache available
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      
      #Get matrix to compute
      data <- x$getMatrix()
      
      #Compute the inverse of square matrix
      i <- solve(data, ...)
      
      #Set the cache for next time
      x$setInverseCache(i)
      
      #Return the inverse
      i
}
